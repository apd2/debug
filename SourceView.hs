module SourceView() where

import qualified Data.Graph.Inductive.Graph as Graph
import qualified Graphics.UI.Gtk            as G

import Util
import qualified DbgTypes as D
import NS
import qualified CFASpec  as C
import qualified IExpr    as I
import qualified CFA      as I

--------------------------------------------------------------
-- Data structures
--------------------------------------------------------------

-- Variable assignments
Store = SStruct [(String, Store)] -- name/value pairs (used for structs and for top-level store)
      | SArr    [Store]           -- array assignment
      | SVal    Maybe I.Val       -- scalar


storeGetLoc :: Store -> I.PID -> Maybe String -> I.Loc
storeGetLoc store pid methname = pcEnumToLoc pc
    where pcname = mkPCVarName $ pid ++ maybeToList methname
          pc     = storeGetScalar s pcname

data TraceEntry = TraceEntry I.Stack Store

type Trace = [TraceEntry]

data SourceView c a = SourceView {
    svSpec         :: C.Spec,
    svPID          :: I.PID,                     -- PID set in the process selection menu
    svState        :: D.State a Store,           -- Current state set via view callback
    svTrace        :: Trace,                     -- Steps from the current state
    svTracePos     :: Int,                       -- Current position in the trace

    -- Process selector
    svProcessCombo :: G.ComboBox,                -- process selection combo box
    svProcessStore :: G.TreeStore (I.PID, Bool), -- tree store that backs the process selector

    -- Stack view
    svStackView    :: G.TreeView,                -- stack view
    svStackStore   :: G.ListStore Frame,         -- store containing list of stack frames
    svStackFrame   :: Int                        -- selected stack frame (0 = top)
}

--------------------------------------------------------------
-- View callbacks
--------------------------------------------------------------

sourceViewNew :: (D.Rel c v a s) => C.Spec -> D.RModel c a Store -> IO (D.View a Store)
sourceViewNew spec model = do
    ref = newIORef $ SourceView { svSpec         = spec
                                , svPID          = error "SourceView: PID undefined"
                                , svState        = error "SourceView: state undefined"
                                , svTrace        = []
                                , svTracePos     = 0
                                , svProcessCombo = error "SourceView: svProcessCombo undefined"
                                , svProcessStore = error "SourceView: svProcessStore undefined"
                                , svStackView    = error "SourceView: svStackView undefined"
                                , svStackStore   = error "SourceView: svStackStore undefined"
                                , svStackFrame   = error "SourceView: svStackFrame undefined"
                                }

    vbox <- G.vBoxNew False 0
    G.widgetShow vbox

    -- toolbar at the top
    tbar <- G.toolbarNew
    G.boxPackStart vbox tbar G.PackNatural 0
    G.widgetShow tbar

    -- process selector
    sel <- processSelectorCreate ref
    selitem <- G.toolItemNew
    G.widgetShow selitem
    G.containerAdd selitem tbar
    G.toolbarInsert tbar selitem (-1)

    -- command buttons
    butstep <- G.toolButtonNewFromStock G.stockGoForward
    G.onToolButtonClicked butstep (step ref)
    G.widgetShow butstep
    G.toolbarInsert tbar butstep (-1)

    butrun <- G.toolButtonNewFromStock G.stockMediaPlay
    G.onToolButtonClicked butrun (run ref)
    G.widgetShow butrun
    G.toolbarInsert tbar butst (-1)

    -- trace
    tr <- traceViewCreate ref
    tritem <- G.toolItemNew
    G.widgetShow tritem
    G.containerAdd tritem tbar
    G.toolbarInsert tbar tritem (-1)

    -- horizontal PanedPanels
    hpanes <- panedPanelsNew (liftM G.toPaned $ G.hPanedNew)
    w <- panelsGetWidget hpanes
    G.boxPackStart vbox w G.PackGrow 0

    -- source window on the left
    src <- sourceWindowCreate ref
    panelsAppend hpanes src "Source Code"

    -- stack, watch on the right
    vpanes <- panefPanelsNew (liftM G.toPaned $ G.vPanedNew)
    panelsAppend hpanes vpanes ""
    stack <- stackViewCreate ref
    watch <- watchCreate ref
    panelsAppend vpanes stack "Stack"
    panelsAppend vpanes watch "Watch"

    let cb = D.ViewEvents { D.evtStateSelected      = sourceViewStateSelected      ref 
                          , D.evtTransitionSelected = sourceViewTransitionSelected ref
                          }

    -- disable everything until we get a state to start from
    disable ref

    return $ D.View { D.viewName      = "Source"
                    , D.viewDefAlign  = D.AlignCenter
                    , D.viewShow      = return ()
                    , D.viewHide      = return ()
                    , D.viewGetWidget = return $ G.toWidget vbox
                    , D.viewCB        = cb
                    }
    

sourceViewStateSelected :: (D.Rel c v a s) => RSourceView c a -> Maybe (D.State a Store) -> IO ()
sourceViewStateSelected ref Nothing                              = disable ref
sourceViewStateSelected ref (Just s) | isNothing (D.sConcrete s) = disable ref
                                     | otherwise                 = do
    modifyIORef ref (\sv -> sv{svState = s})
    reset ref

sourceViewTransitionSelected :: (D.Rel c v a s) => RSourceView c a -> D.Transition a Store -> IO ()
    -- ignore abstract transitions
    -- run transition with automatic determinisation



--------------------------------------------------------------
-- Actions
--------------------------------------------------------------

-- Execute one statement
step :: RSourceView c a -> IO Bool
step ref = do
    modifyIORef ref (\sv -> sv{svStackFrame = 0})
    -- save current state
    backup <- readIORef ref
    ok <- microstep ref
    if ok
       then do -- did we reach the next statement?
               action <- getIORef (I.locAct . currentLocLabel) ref
               case action of
                    ActNone -> step ref
                    _       -> do -- if we reached a pause statement, update PC and PID variables
                                  lab <- getIORef currentLocLabel ref
                                  when (isDelayLabel lab) $ completeTransition ref
                                  updateDisplays ref
                                  return True 
       else do -- rollback changes
               writeIORef ref backup
               updateDisplays ref
               return False



-- run until pause or nondeterministic choice
run :: RSourceView c a -> IO ()
run ref = do
    ok <- step ref
    lab <- getIORef currentLocLabel ref
    when (ok && (not $ isDelayLabel lab)) $ run ref


--------------------------------------------------------------
-- Components
--------------------------------------------------------------

-- Process selector --
processSelectorCreate :: RSourceView c a  -> IO G.Widget
processSelectorCreate ref = do
    sv <- readIORef ref
    combo <- G.comboBoxNew
    G.widgetShow combo
    store <- G.treeStoreNew []
    rend <- G.cellRendererTextNew
    G.cellLayoutPackStart combo rend True
    G.cellLayoutSetAttributeFunc combo rend store $ 
        (\iter -> path <- G.treeModelGetPath store iter
                  (pid, en) <- G.treeStoreGetValue store path
                  set rend [cellTextMarkup := "<span weight=" ++ if en then "bold" else "normal" ++ ">" ++ last pid ++ "</span>"])
    writeIORef ref sv {svProcessCombo = combo, svProcessStore = store}
    G.on combo G.changed (processSelectorChanged ref)
    return $ G.toWidget combo

processSelectorChanged :: RSourceView c a -> IO ()
processSelectorChanged ref = do
    sv <- readIORef ref
    miter <- comboBoxGetActiveIter $ svProcessCombo sv
    when isJust miter $ 
        do path <- G.treeModelGetPath (svProcessStore sv) (fromJust iter)
           (pid, en) <- G.treeStoreGetValue (svProcessStore sv) path
           modifyIORef ref (\sv -> sv{svPID = pid})
           reset ref

processSelectorDisable :: RSourceView c a -> IO ()
processSelectorDisable ref = do
    combo <- getIORef svProcessCombo ref
    G.widgetSetSensitive combo False

processSelectorUpdate :: RSourceView c a -> IO ()
processSelectorUpdate ref = do
    sv <- readIORef ref
    let combo = svProcessCombo sv
        store = svProcessStore sv
        pidtree = map (procTree []) (C.specProc $ svSpec sv)
                  where procTree parpid p = Tree { rootLabel = (pid, isProcEnabled sv pid)
                                                 , subForest = map (procTree pid) (procChildren p)}
                                            where pid = parpid ++ [procName p]
    G.treeStoreClear store
    G.treeStoreInsertForest store [] 0 pidtree 
    G.widgetSetSensitive combo True


-- Stack --
stackViewCreate :: RSourceView c a -> IO G.Widget
stackViewCreate ref = do
    view <- G.treeViewNew
    G.widgetShow view
    store <- G.listStoreNew []
    rend <- G.cellRendererTextNew
    G.cellLayoutPackStart view rend True
    G.cellLayoutSetAttributeFunc view rend store $ 
        (\iter -> idx <- G.listStoreIterToIndex iter
                  frame <- G.listStoreGetValue store idx
                  set rend [cellTextMarkup := sname $ fScope frame)
    modifyIORef ref (\sv -> sv{svStackView = view, svStackStore = store, svStackFrame = 0})
    G.treeViewSetModel view store
    G.on view rowActivated (stackViewFrameSelected ref)
    return $ G.toWidget view

stackViewFrameSelected :: RSourceView c a -> TreePath -> TreeViewColumn -> IO ()
stackViewFrameSelected ref (idx:_) _ = do
    modifyIORef ref (\sv -> sv{svStackFrame = idx})
    sourceWindowUpdate ref
    watchUpdate ref

stackViewUpdate :: RSourceView c a -> IO ()
stackViewUpdate ref = do
    sv <- readIORef ref
    let view = svStackView sv
        store = svStackStore sv
    G.listStoreClear store
    mapM (listStoreAppend store) $ currentStack sv
    return ()

stackViewDisable :: RSourceView c a -> IO ()
stackViewDisable ref = do
    sv <- readIORef ref
    G.listStoreClear $ svStackStore sv

-- Trace --
traceViewCreate ref
traceViewDisable ref
traceViewUpdate ref

-- Watch --
watchCreate ref
watchUpdate ref
watchDisable ref


-- Source --
sourceWindowUpdate ref
sourceWindowDisable ref

-- Command buttons --

commandButtonsUpdate ref

commandButtonsDisable ref
    -- disable command buttons
    G.widgetSetSensitive (svStepButton sv) False
    G.widgetSetSensitive (svRunButton sv) False


--------------------------------------------------------------
-- Private helpers
--------------------------------------------------------------

-- Given a snapshot of the store at a pause location, compute
-- process stack.
stackFromStore :: Store -> I.PID -> Stack
stackFromStore s pid = 
    -- concatenate stacks from each process down the branch of the process tree
    concat $ reverse $ map (\pid -> stackFromStore' s pid Nothing) $ tail $ inits pid

stackFromStore' :: Store -> I.PID -> Maybe String -> Stack
stackFromStore' s pid methname = stack' ++ stack
    where cfa    = C.getCFA pid methname
          loc    = storeGetLoc store pid methname
          label  = cfaLocLabel loc cfa
          stack  = locStack label
          -- If this location corresponds to a task call, recurse into task's CFA
          stack' = case label of 
                        LPause _ _ e -> case isWaitForTask e of
                                             Nothing   -> []
                                             Just name -> stackFromStore' s pid (Just name)
                        _            -> []


isProcEnabled :: SourceView c a -> I.PID -> Bool

-- update all displays
updateDisplays :: RSourceView c a -> IO ()
updateDisplays ref = do
    commandButtonsUpdate ref
    stackViewUpdate      ref
    traceViewUpdate      ref
    watchUpdate          ref
    sourceWindowUpdate   ref

-- Reset all components
reset :: RSourceView c a -> IO ()
reset ref = do
    processSelectorUpdate ref
    sv <- readIORef ref
    -- initialise stack and trace
    let trace = [stackFromStore (D.sConcrete s) (svPID sv)]
    writeIORef ref sv{svTrace = trace, svTracePos = 0, svStackFrame = 0}
    updateDisplays ref

-- Disable all controls
disable :: RSourceView c a -> IO ()
disable ref = do
    sv <- readIORef ref
    -- disable process selector
    processSelectorDisable ref
    -- disable other displays
    commandButtonsDisable ref
    sourceWindowDisable ref
    stackViewDisable ref
    traceViewDisable ref
    watchDisable ref


currentCFA :: SourceView -> I.CFA

currentLoc :: SourceView -> I.Loc

currentLocLabel :: SourceView -> I.LocLabel

currentStore :: SourceView -> Store

currentStack :: SourceView -> I.Stack

-- Execute one CFA transition
-- Returns True if the step was performed successfully and
-- False otherwise (i.e., the user did not provide values
-- for nondeterministic arguments)
microstep :: RSourceView c a -> IO Bool
microstep ref = do
    sv <- readIORef ref
    -- Try all transitions from the current location; choose the first successful one
    let transitions = Graph.lsuc (currentCFA sv) (currencLoc sv)
    sv' <- foldM resolveTran sv transitions
    case mapMaybe (microstep' sv') transitions of
         []                      -> return False
         (Just (store, stack)):_ -> do writeIORef ref sv' $ traceAppend sv store stack
                                       return True

microstep' :: SourceView c a -> (I.Loc, I.TranLabel) -> Maybe (Store, I.Stack)
microstep' sv (to, TranCall scope)         = Just (currentStore sv, (Frame scope to) : currentStack sv)
microstep' sv (to, TranReturn)             = Just (currentStore sv, tail $ currentStack sv)
microstep' sv (to, TranNop)                = Just (currentStore sv, (head $ currentStack sv){fLoc = to} : (tail $ currentStack sv))
microstep' sv (to, TranStat (SAssume e))   = case eval e of
                                                  BoolVal True -> Just (currentStore sv, (head $ currentStack sv){fLoc = to} : (tail $ currentStack sv))
                                                  _            -> Nothing
microstep' sv (to, TranStat (SAssign l r)) = Just (currentStore sv, (head $ currentStack sv){fLoc = to} : (tail $ currentStack sv))
                                             where store' = storeSet (currentStore sv) l (eval r)

resolveTran :: SourceView c a -> (I.Loc, I.TranLabel) -> IO (SourceView c a)
resolveTran sv (_, TranStat (SAssume e))   = resolveExpr sv e
resolveTran sv (_, TranStat (SAssign l r)) = resolveExpr sv r
resolveTran sv _                           = return sv
