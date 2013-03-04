module SourceView() where

import qualified Data.Graph.Inductive.Graph as Graph
import qualified Graphics.UI.Gtk            as G

import Util
import qualified DbgTypes as D
import Store
import NS
import qualified CFASpec  as C
import qualified IExpr    as I
import qualified CFA      as I

--------------------------------------------------------------
-- Data structures
--------------------------------------------------------------

-- process stack, including uncontrollable and controllable task stacks
data PStack = [(I.Frame, (I.PID, Maybe String))]

data TraceEntry = TraceEntry {
    teStack :: PStack,
    teStore :: Store
}

type Trace = [TraceEntry]

data SourceView c a = SourceView {
    svModel        :: D.RModel a Store
    svSpec         :: C.Spec,
    svAbsVars      :: M.Map String AbsVar,
    svState        :: D.State a Store,            -- current state set via view callback
    svTmp          :: Store,                      -- temporary variables store
    svTranPID      :: Maybe I.PID,                -- set by transitionSelected, restricts enabled processes to PID

    -- Trace
    svTrace        :: Trace,                      -- steps from the current state
    svTracePos     :: Int,                        -- current position in the trace
    svTraceCombo   :: G.ComboBox,                 -- trace combo box
    svTraceStore   :: G.ListStore Int,            -- indices in the trace that correspond to visible locations
    svTraceUndo    :: G.ToolButton,               -- back button
    svTraceRedo    :: G.ToolButton,               -- forward button

    -- Process selector
    svPID          :: I.PID,                      -- PID set in the process selection menu
    svProcessCombo :: G.ComboBox,                 -- process selection combo box
    svProcessStore :: G.TreeStore (I.PID, Bool),  -- tree store that backs the process selector

    -- Stack view
    svStackView    :: G.TreeView,                 -- stack view
    svStackStore   :: G.ListStore Frame,          -- store containing list of stack frames
    svStackFrame   :: Int,                        -- selected stack frame (0 = top)

    -- Watch
    svWatchView    :: G.TreeView,
    svWatchStore   :: G.ListStore (Maybe String), -- store containing watch expressions

    -- Source window
    svSourceView   :: G.SourceView,
    svSourceTag    :: G.TextTag,                  -- tag to mark current selection current selection

    -- Resolve view
    svResolveStore :: G.TreeStore I.Expr          -- tmp variables in the scope of the current expression
}

--------------------------------------------------------------
-- View callbacks
--------------------------------------------------------------

sourceViewNew :: (D.Rel c v a s) => C.Spec -> [AbsVar] -> D.RModel c a Store -> IO (D.View a Store)
sourceViewNew spec avars model = do
    ref = newIORef $ SourceView { svModel        = model
                                , svSpec         = C.specInlineWireAlways spec
                                , svAbsVars      = M.fromList $ map (\v -> (show v, v)) avars
                                , svState        = error "SourceView: state undefined"
                                , svTmp          = error "SourceView: svTmp undefined"
                                , svTranPID      = Nothing,
                                , svTrace        = []
                                , svTracePos     = 0
                                , svTraceCombo   = error "SourceView: svTraceCombo undefined"
                                , svTraceStore   = error "SourceView: svTraceStore undefined"
                                , svTraceUndo    = error "SourceView: svTraceUndo undefined"
                                , svTraceRedo    = error "SourceView: svTraceRedo undefined"
                                , svPID          = error "SourceView: PID undefined"
                                , svProcessCombo = error "SourceView: svProcessCombo undefined"
                                , svProcessStore = error "SourceView: svProcessStore undefined"
                                , svStackView    = error "SourceView: svStackView undefined"
                                , svStackStore   = error "SourceView: svStackStore undefined"
                                , svStackFrame   = error "SourceView: svStackFrame undefined"
                                , svWatchStore   = error "SourceView: svWatchStore undefined"
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

    -- stack, watch, resolve on the right
    vpanes <- panefPanelsNew (liftM G.toPaned $ G.vPanedNew)
    panelsAppend hpanes vpanes ""
    stack <- stackViewCreate ref
    watch <- watchCreate ref
    resolve <- resolveViewCreate ref
    panelsAppend vpanes stack "Stack"
    panelsAppend vpanes watch "Watch"
    panelsAppend vpanes resolve "Resolve non-determinism"

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
    modifyIORef ref (\sv -> sv { svState   = s
                               , svTmp     = emptyTmpStore sv
                               , svTranPID = Nothing})
    reset ref

sourceViewTransitionSelected :: (D.Rel c v a s) => RSourceView c a -> D.Transition a Store -> IO ()
sourceViewTransitionSelected ref tran | isNothing (D.sConcrete $ D.tranFrom tran) = disable ref
                                      | otherwise                                 = do
    modifyIORef ref (\sv -> sv { svState   = D.tranFrom tran
                               , svTmp     = tranTmpStore tran sv
                               , svTranPID = storeEvalEnum (fromJust $ D.sConcrete $ D.tranTo tran) mkPIDVar})
    reset ref

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
-- GUI components
--------------------------------------------------------------

-- Process selector --
processSelectorCreate :: RSourceView c a -> IO G.Widget
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

    col <- G.treeViewColumnNew
    G.treeViewColumnSetTitle col "Stack frames"

    rend <- G.cellRendererTextNew
    G.cellLayoutPackStart col rend True
    G.cellLayoutSetAttributeFunc col rend store $ 
        (\iter -> idx <- G.listStoreIterToIndex iter
                  frame <- G.listStoreGetValue store idx
                  set rend [cellTextMarkup := sname $ fScope frame)
    G.treeViewAppendColumn view col

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
traceViewCreate :: RSourceView c a -> IO G.Widget
traceViewCreate ref = do
    hbox <- G.hBoxNew False 0
    G.widgetShow hbox

    -- undo button
    undo <- G.toolButtonNewFromStock G.stockUndo
    G.widgetShow undo
    G.boxPackStart hbox undo G.PackNatural 0
    G.onToolButtonClicked undo (do modifyIORef ref (\sv -> traceSetPos sv (svTracePos sv - 1))
                                   updateDisplays ref)

    -- trace
    combo <- G.comboBoxNew
    G.widgetShow combo
    store <- G.treeStoreNew []
    rend <- G.cellRendererTextNew
    G.cellLayoutPackStart combo rend True
    G.cellLayoutSetAttributeFunc combo rend store $ 
        (\iter -> path <- G.treeModelGetPath store iter
                  idx  <- G.treeStoreGetValue store path
                  sv   <- readIORef ref
                  let txt = take 48 $ show $ replace "\n" " " text $
                            case locAct $ getLocLabel sv idx of
                                 ActStat s -> pack $ show s
                                 ActExpr e -> pack $ show e
                                 _         -> "?"
                  set rend [cellTextMarkup := "<span weight=" ++ if idx == svTracePos sv then "bold" else "normal" ++ ">" ++ txt ++ "</span>"])
    G.on combo G.changed (tracePosChanged ref)

    -- redo button
    redo <- G.toolButtonNewFromStock G.stockRedo
    G.widgetShow redo
    G.boxPackStart hbox redo G.PackNatural 0
    G.onToolButtonClicked redo (do modifyIORef ref (\sv -> traceSetPos sv (svTracePos sv + 1))
                                   updateDisplays ref)

    modifyIORef ref $ (\sv -> sv { svTraceStore = store
                                 , svTraceCombo = combo
                                 , svTraceUndo  = undo
                                 , svTraceRedo  = redo})

    return $ G.toWidget hbox

tracePosChanged :: RSourceView c a -> IO ()
tracePosChanged ref = do
    sv <- readIORef ref
    miter <- comboBoxGetActiveIter $ svTraceCombo sv
    when isJust miter $ 
        do storeidx <- G.listStoreIterToIndex $ fromJust miter
           pos <- G.listStoreGetValue (svTraceStore sv) storeidx
           modifyIORef ref (\sv -> traceSetPos sv pos)
           updateDisplays ref


traceViewDisable :: RSourceView c a -> IO ()
traceViewDisable ref = do
    sv <- readIORef ref
    G.listStoreClear $ svTraceStore sv
    G.widgetSetSensitive (svTraceUndo sv) False
    G.widgetSetSensitive (svTraceRedo sv) False
    G.widgetSetSensitive (svTraceCombo sv) False

traceViewUpdate :: RSourceView c a -> IO ()
traceViewUpdate ref = do
    sv <- readIORef ref
    -- update store
    G.listStoreClear $ svTraceStore sv
    mapM (listStoreAppend (svTraceStore sv) . snd) 
         $ filter (\(t,i) -> case locAct $ getLocLabel sv i
                                  ActNone -> False
                                  _       -> True)
         $ zip (svTrace sv) [0..]

    -- set selection
    items <- G.listStoreToList (svTraceStore sv)
    case findIndex (svTracePos sv ==) item of
         Nothing -> G.comboBoxSetActive (svTraceCombo sv) (-1)
         Just i  -> G.comboBoxSetActive (svTraceCombo sv) i

    -- enable/disable buttons
    G.widgetSetSensitive (svTraceUndo sv) (svTracePos sv /= 0)
    G.widgetSetSensitive (svTraceRedo sv) (svTracePos sv /= length (svTrace sv) - 1)


traceAppend :: SourceView c a -> Store -> PStack -> SourceView c a
traceAppend sv store stack = sv {svTrace = tr, svTracePos = pos}
    where tr  = take (svTracePos sv + 1) (svTrace sv) ++ [TraceEntry stack store]
          pos = length tr - 1

traceSetPos :: SourceView c a -> Int -> SourceView c a
traceSetPos sv i | (i >= length $ svTrace sv) || (i < 0) = sv
                 | otherwise = sv {svTracePos = i}


-- Watch --

watchCreate :: RSourceView c a -> IO G.Widget
watchCreate ref = do
    view <- G.treeViewNew
    G.widgetShow view
    G.boxPackStart vbox view G.PackGrow 0

    store <- G.treeStoreNew [Nothing]

    namecol <- G.treeViewColumnNew
    G.treeViewColumnSetTitle namecol "Watch expression"

    exprend <- G.cellRendererTextNew
    G.cellLayoutPackStart namecol exprend False
    G.cellLayoutSetAttributeFunc namecol exprend store $ 
        (\iter -> idx  <- G.listStoreIterToIndex iter
                  mexp <- G.listStoreGetValue store idx
                  set exprend [ cellTextEditable := True
                              , cellTextMarkup := case mexp of 
                                                       Nothing  -> "<i>Add watch</i>"
                                                       Just exp -> exp])
    G.on exprend G.edited (\_ _ -> watchUpdate ref)
    G.treeViewAppendColumn view namecol

    valcol <- G.treeViewColumnNew
    G.treeViewColumnSetTitle valcol "Value"

    valrend <- G.cellRendererTextNew
    G.cellLayoutPackStart valcol valrend True
    G.cellLayoutSetAttributeFunc valcol valrend store $ 
        (\iter -> sv <- readIORef ref
                  idx  <- G.listStoreIterToIndex iter
                  mexp <- G.listStoreGetValue store idx
                  set valrend [cellTextMarkup := case mexp of 
                                                      Nothing  -> ""
                                                      Just exp -> case storeEvalStr (currentStore sv) (currentStack sv !! svStackFrame sv) exp of
                                                                       Left e  -> e
                                                                       Right v -> show v])
    G.treeViewAppendColumn view valcol

    G.on view G.keyPressEvent (do key <- eventKeyVal
                                  when (key == "Delete") $ liftIO $ watchDelete ref)
    modifyIORef ref (\sv -> sv{svWatchView = view, svWatchStore = store})

    G.treeViewSetModel view store
    return $ G.toWidget view

watchDelete :: RSourceView c a -> IO ()
watchDelete ref = do
    sv <- readIORef ref
    (idx:_, _) <- G.treeViewGetCursor (svWatchView sv)
    G.listStoreRemove (svWatchStore sv) idx
    watchUpdate ref

watchUpdate :: RSourceView c a -> IO ()
watchUpdate ref = do
    store <- getIORef svWatchStore ref
    items <- G.listStoreToList store
    -- make sure that there is an empty slot in the end
    let items' = (filter isJust items) ++ [Nothing]
    -- refill the list to force watch update
    G.listStoreClear store
    mapM (G.listStoreAppend store) items'
    return ()

watchDisable :: RSourceView c a -> IO ()
watchDisable _ = return ()


-- Source --
sourceWindowCreate :: RSourceView c a -> IO G.Widget
sourceWindowCreate ref = do
    view <- G.textViewNew
    G.widgetShow view
    tag <- G.textTagNew
    G.set tag [textTagBackground := "grey"]
    modifyIORef ref (\sv -> sv {svSourceView = view, svSourceTag = tag})
    return $ G.toWidget view


sourceWindowUpdate :: RSourceView c a -> IO ()
sourceWindowUpdate ref = do
    sv <- readIORef ref
    let cfa = cfaAtFrame sv (svStackFrame sv)
        loc = fLoc $ fst $ (currentStack sv) !! (svStackFrame sv)
        p = case locAct $ I.cfaLocLabel cfa loc of
                 ActNone   -> return ()
                 ActExpr e -> sourceSetPos sv $ pos e
                 ActStat s -> sourceSetPos sv $ pos s

sourceWindowDisable :: RSourceView c a -> IO ()
sourceWindowDisable _ = return ()

sourceSetPos :: SourceView c a -> Pos -> IO ()
sourceSetPos sv (from, to) = do
    let fname = sourceName from
    do src <- readFile fname
       buf <- G.textBufferNew Nothing
       G.textBufferSetText buf src
       G.textViewSetBuffer (svSourceView sv) buf
       istart <- G.textBufferGetStartIter buf
       iend <- G.textBufferGetEndIter buf
       ifrom <- G.textBufferGetIterAtLineOffset buf (sourceLine from) (sourceColumn from)
       ito <- G.textBufferGetIterAtLineOffset buf (sourceLine to) (sourceColumn to)
       G.textBufferRemoveTag buf (svSourceTag sv) istart iend
       G.textBufferApplyTag buf (svSourceTag sv) ifrom ito
       G.textViewScrollToIter (svSourceView sv) ifrom 0.5 Nothing
    `catch` (\e -> return ())



-- Command buttons --
commandButtonsUpdate :: RSourceView c a -> IO ()
commandButtonsUpdate ref = do
    sv <- readIORef ref
    -- enable step and run buttons if we are not at a pause location or
    -- if we are in a pause location and the wait condition is true
    let lab = currentLocLabel sv
        en = case lab of
                  LInst _      -> True
                  LPause _ _ c -> storeEvalBool (currentStore sv) c == True
                  LFinal _ _   -> True
    G.widgetSetSensitive (svStepButton sv) en
    G.widgetSetSensitive (svRunButton sv) en

commandButtonsDisable ref
    -- disable command buttons
    G.widgetSetSensitive (svStepButton sv) False
    G.widgetSetSensitive (svRunButton sv) False

-- Resolve --

resolveViewCreate :: RSourceView c a -> IO G.Widget
resolveViewCreate ref = do

    view <- G.treeViewNew
    G.widgetShow view
    store <- G.treeStoreNew []

    -- Variable name column
    namecol <- G.treeViewColumnNew
    G.treeViewColumnSetTitle namecol "Variables"

    namerend <- G.cellRendererTextNew
    G.cellLayoutPackStart namecol namerend False
    G.cellLayoutSetAttributeFunc namecol namerend store $ 
        (\iter -> sv <- readIORef ref
                  path <- G.treeModelGetPath store iter
                  exp <- G.treeStoreGetValue store path
                  let hl = isNothing $ storeEval (currentStore sv) exp
                  set namerend [cellTextMarkup := if hl 
                                                     then "<span background=\"red\">" ++ show exp ++ "</span>" 
                                                     else show exp])
    G.treeViewAppendColumn view namecol

    -- Variable assignment column
    valcol <- G.treeViewColumnNew
    G.treeViewColumnSetTitle valcol "Value"

    textrend <- G.cellRendererTextNew
    G.cellLayoutPackStart valcol textrend True
    G.cellLayoutSetAttributeFunc valcol textrend store $ 
        (\iter -> sv <- readIORef ref
                  path <- G.treeModelGetPath store iter
                  exp <- G.treeStoreGetValue store path
                  set textrend [ cellVisible := isTypeInt exp
                                 cellTextEditable := True
                                 cellText := show $ storeEval (currentStore sv) exp])
    G.on textrend G.edited (textAsnChanged ref)

    combrend <- G.cellRendererComboNew
    G.cellLayoutPackStart valcol combrend True
    G.cellLayoutSetAttributeFunc valcol combrend store $ 
        (\iter -> sv <- readIORef ref
                  path <- G.treeModelGetPath store iter
                  exp <- G.treeStoreGetValue store path
                  set combrend [ cellVisible := isTypeScalar exp && not (isTypeInt exp)
                                 cellComboTextModel := (textModel $ typ exp, G.makeColumnIdString 0)
                                 cellTextEditable := True
                                 cellText := show $ storeEval (currentStore sv) exp])
    G.on combrend G.edited (textAsnChanged ref) 

    G.treeViewAppendColumn view valcol
    modifyIORef ref (\sv -> sv {svResolveStore = store})
    return $ G.toWidget view

comboTextModel :: I.Type -> G.ListStore String
comboTextModel I.Bool     = G.listStoreNew ["*", "True", "False"]
comboTextModel (I.Enum n) = G.listStoreNew ("*": enumEnums $ getEnumeration n)

textAsnChanged :: RSourceView c a -> TreePath -> String -> IO ()
textAsnChanged ref path valstr = do
    sv <- readIORef ref
    exp <- G.treeStoreGetValue (svResolveStore sv) path
    val = if valstr == "*"
             then SVal Nothing
             else case I.parseVal (typ exp) val of
                       Left e  -> do showMessage ref G.MessageError e
                                     return $ SVal Nothing
                       Right v -> SVal $ Just v
    writeIORef ref $ updateCurrentStore sv (\store -> storeSet store exp val)
    resolveViewUpdate ref

resolveViewUpdate :: RSourceView c a -> IO ()
resolveViewUpdate ref = do
    sv <- readIORef ref
    let -- collect tmp variables from all transitions from the current location
        trans = map snd $ Graph.lsuc (currentCFA sv) (currencLoc sv)
        exprs = map (mkTree . I.Evar)
                $ filter (== I.VarTmp . I.varCat)
                $ S.toList $ S.fromList $
                $ concatMap (\t -> case t of
                                        TranStat (SAssume e)   -> I.exprVars e
                                        TranStat (SAssign _ e) -> I.exprVars e
                                        _                      -> []) 
                $ trans
        -- expand expression into a tree of scalars
        mkTree exp = Node { rootLabel = exp
                          , subForest = map mkTree 
                                        $ case typ exp of
                                               I.Struct fs  -> map (\(Field n t) -> I.EField exp n) fs
                                               I.Array t sz -> map (\i -> I.EIndex exp i) [0..sz-1]
                                               _            -> []
                          }
    let store = svResolveStore sv
    G.treeStoreClear store
    G.treeStoreInsertForest store [] 0 exprs


resolveViewDisable :: RSourceView c a -> IO ()
resolveViewDisable ref = do
    sv <- readIORef ref
    G.treeStoreClear $ svResolveStore sv

--------------------------------------------------------------
-- Abstraction/concretisation
--------------------------------------------------------------

abstractTransition :: RSourceView c a -> State a Store -> Store -> IO (Transition a Store)
abstractTransition ref from to =
    SourceView{..} <- readIORef ref
    model <- readIORef $ svModel sv
    let tranFrom = from
        -- compute untracked and label predicates over to
        tranUntracked = conj $ map (evalAbsVar sv to) D.mUntrackedVars
        tranAbstractLabel = conj $ map (evalAbsVar sv to) D.mLabelVars
        -- project "to" state on "tmp" variables
        tranConcreteLabel = Just $ storeProject to (map I.varName $ C.specTmpVars svSpec)
        tranTo = D.State { sAbstract = conj $ map (evalAbsVar sv to . (\(n,t,(i,_)) -> (n,t,i))) D.mStateVars
                         , sConcrete = Just $ storeProject to (map I.varName $ C.specVar svSpec)}
    return $ Transition{..}


evalAbsVar :: SourceView c a -> Store -> (String, Type, [Int]) -> a
evalAbsVar sv store (name, _, is) = evalAbsVar' store (svAbsVars M.! name) is

evalAbsVar' :: Store -> AbsVar -> [Int] -> a
evalAbsVar' store (AVarPred p) is = 
    if storeEvalBool store (predToExpr p)
       then eqConst (D.idxToVS is) 1
       else eqConst (D.idxToVS is) 0

evalAbsVar' store (AVarVar name) is = 
   eqConst (D.idxToVS is) $ scalarToInt $ storeEvalScalar store (I.EVar name)

evalAbsVar' store (AVarEnum name vals) is =
    case findIndex (==v) vals of
         Just i  -> eqConst vs i
         Nothing -> conj $ map (nt $ eqConst vs i) [0..length vals -1]
    where v  = storeEvalScalar store (I.EVar name)
          vs = D.idxToVS is

scalarToInt :: I.Val -> Int
scalarToInt (I.BoolVal True)  = 1
scalarToInt (I.BoolVal False) = 0
scalarToInt (I.UIntVal i)     = i
scalarToInt (I.SIntVal i)     = i
scalarToInt (I.EnumVal s)     = fromJust $ findIndex (== s) $ enumEnums $ getEnumerator s


--------------------------------------------------------------
-- Private helpers
--------------------------------------------------------------

-- Given a snapshot of the store at a pause location, compute
-- process stack.
stackFromStore :: Store -> I.PID -> PStack
stackFromStore s pid = stackFromStore' s pid Nothing

-- Returns extended version of the stack with PID and task name 
-- attached to each frame
stackFromStore' :: Store -> I.PID -> Maybe String -> PStack
stackFromStore' s pid methname = stack' ++ stack
    where cfa    = C.specGetCFA (svSpec sv) pid methname
          loc    = storeGetLoc store pid methname
          label  = I.cfaLocLabel loc cfa
          stack  = zip (locStack label) (repeat (pid, methname))
          -- If this location corresponds to a task call, recurse into task's CFA
          stack' = case label of 
                        LPause _ _ e -> case isWaitForTask e of
                                             Nothing   -> if isWaitForMagic e
                                                             then case getTag s of
                                                                       Nothing -> []
                                                                       Just t  -> stackFromStore' s [] (Just t)
                                                             else []
                                             Just name -> stackFromStore' s pid (Just name)
                        _            -> []

storeGetLoc :: Store -> I.PID -> Maybe String -> I.Loc
storeGetLoc store pid methname = pcEnumToLoc pc
    where pcvar = mkPCVar $ pid ++ maybeToList methname
          pc    = storeEvalEnum s pcvar


getTag :: Store -> String
getTag s = storeEvalEnum s mkTagVar

isProcEnabled :: SourceView c a -> I.PID -> Bool
isProcEnabled sv pid = 
    let store = D.sConcrete $ svState sv
        (frame, (pid, mmeth)) = head $ stackFromStore' store pid Nothing
        cfa = C.specGetCFA (svSpec sv) pid mmeth
    in case svTranPID sv of
            Just pid' -> pid' == pid
            _         -> case cfaLocLabel (fLoc frame) cfa of
                              LPause _ _ cond -> storeEvalBool store cond == True
                              _               -> True

-- update all displays
updateDisplays :: RSourceView c a -> IO ()
updateDisplays ref = do
    commandButtonsUpdate ref
    stackViewUpdate      ref
    traceViewUpdate      ref
    watchUpdate          ref
    sourceWindowUpdate   ref
    resolveViewUpdate    ref

-- Reset all components
reset :: RSourceView c a -> IO ()
reset ref = do
    processSelectorUpdate ref
    sv <- readIORef ref
    -- initialise stack and trace
    let trace = [TraceEntry { teStack = stackFromStore (D.sConcrete $ svState sv) (svPID sv)
                            , teStore = storeUnion (D.sConcrete $ svState sv) (svTmp sv)}]
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
    resolveViewDisable ref

-- Access current location in the trace 

currentCFA :: SourceView c a -> I.CFA
currentCFA sv = getCFA sv (svTracePos sv)

currentLoc :: SourceView c a -> I.Loc
currentLoc sv = getLoc sv (svTracePos sv)

currentLocLabel :: SourceView c a -> I.LocLabel
currentLocLabel sv = getLocLabel sv (svTracePos sv)

currentStore :: SourceView c a -> Store
currentStore sv = getStore sv (svTracePos sv)

updateCurrentStore :: SourceView c a -> (Store -> Store) -> SourceView c a

currentStack :: SourceView c a -> PStack
currentStack sv = getStack sv (svTracePos sv)

cfaAtFrame :: SourceView c a -> Int -> I.CFA
cfaAtFrame sv frame = C.specGetCFA (svSpec sv) pid mmeth
    where (pid, mmeth) = snd $ currentStack sv !! frame

-- Access arbitrary location in the trace 

getCFA :: SourceView c a -> Int -> I.CFA
getCFA sv pos = C.specGetCFA (svSpec sv) pid mmeth
    where (pid, mmeth) = snd $ head $ getStack sv pos

getLoc :: SourceView c a -> Int -> I.Loc
getLoc sv pos = fLoc $ fst $ head $ getStack sv pos

getLocLabel :: SourceView c a -> Int -> I.LocLabel
getLocLabel sv pos = I.cfaLocLabel (getLoc sv pos) (getCFA sv pos)

getStore :: SourceView c a -> Int -> Store
getStore sv pos = teStore $ svTrace sv !! pos

getStack :: SourceView c a -> Int -> PStack
getStack sv pos = teStack $ svTrace sv !! pos

-- Execute one CFA transition
-- Returns True if the step was performed successfully and
-- False otherwise (i.e., the user did not provide values
-- for nondeterministic arguments)
microstep :: RSourceView c a -> IO Bool
microstep ref = do
    sv <- readIORef ref
    -- Try all transitions from the current location; choose the first successful one
    let transitions = Graph.lsuc (currentCFA sv) (currencLoc sv)
    case mapMaybe (microstep' sv) transitions of
         []                      -> return False
         (Just (store, stack)):_ -> do writeIORef ref $ traceAppend sv store stack
                                       return True

microstep' :: SourceView c a -> (I.Loc, I.TranLabel) -> Maybe (Store, PStack)
microstep' sv (to, TranCall scope)         = Just (currentStore sv, (Frame scope to) : currentStack sv)
microstep' sv (to, TranReturn)             = Just (currentStore sv, tail $ currentStack sv)
microstep' sv (to, TranNop)                = Just (currentStore sv, (head $ currentStack sv){fLoc = to} : (tail $ currentStack sv))
microstep' sv (to, TranStat (SAssume e))   = if storeEvalBool (currentStore sv) e == True
                                                  then Just (currentStore sv, (head $ currentStack sv){fLoc = to} : (tail $ currentStack sv))
                                                  else Nothing
microstep' sv (to, TranStat (SAssign l r)) = Just (store', (head $ currentStack sv){fLoc = to} : (tail $ currentStack sv))
                                             where store' = storeSet (currentStore sv) l (storeEval (currentStore sv) r)

-- Actions taken upon reaching a delay location
completeTransition :: RSourceView c a -> IO ()
completeTransition ref = do
    sv <- readIORef ref
    -- update PC and PID variables
    let pc = currentLoc sv
        (p, mmeth) = snd $ head $ currentStack sv
        pid = p ++ (maybeToList mmeth)
        sv' = setPC pid pc $ setPID pid sv
    writeIORef ref sv'
    -- abstract final state
    model <- readIORef $ svModel sv'
    let trans = abstractTransition model (svState sv') (currentStore sv')
    -- add transition
    modelSelectTransition (svModel sv') trans
    modelSelectState (svModel sv') (tranTo trans)

setPID :: I.PID -> SourceView c a -> SourceView c a
setPID pid sv = modifyCurrentStore sv $ (\s -> storeSet s mkPIDVar (SVal $ Just $ I.EnumVal $ mkPIDEnumeratorName pid))
    
setPC :: I.PID -> I.Loc -> SourceView c a -> SourceView c a
setPC pid pcloc sv = modifyCurrentStore sv $ (\s -> storeSet s (mkPCVar pid) (SVal $ Just $ I.EnumVal $ mkPCEnum pid pcloc))

-- Evaluate expression written in terms of variables in the original input spec.
evalStr :: Store -> PFrame -> String -> Either String Store
evalStr store (frame, (pid, mmeth)) str = do
    -- Apply all transformations that the input spec goes through to the expression:
    -- 1. parse
    expr <- parse detexpr "" tsl str
    let ?spec = inspec
    let (scope,iid) = case fScope frame of
                            ScopeMethod   _ meth -> let (iid, mname) = itreeParseName $ name meth
                                                        tm = itreeTemplate iid
                                                        meth = fromJust $ find (== mname . methName) $ tmAllMethod tm
                                                    in (ScopeMethod tm meth, iid)
                            ScopeProcess  _ proc -> let (iid, pname) = itreeParseName $ name meth
                                                        tm = itreeTemplate iid
                                                        proc = fromJust $ find (== pname . procName) $ tmAllProcess tm
                                                    in (ScopeProcess tm proc, iid)
                            ScopeTemplate _      -> (ScopeTop, [])
    -- 2. validate
    validateExpr scope expr
    let ?scope = scope in when (not exprNoSideEffects expr) $ throwError "Expression has side effects"
    -- 3. flatten
    let flatexpr = exprFlatten iid scope expr
    -- 4. simplify
    let ?spec = flatspec 
    let (ss, simpexpr) = let ?uniq = newUniq
                             ?scope = fScope frame
                         in exprSimplify flatexpr
    when (ss /= []) $ throwError "Expression too complex"
    -- 5. inline
    let lmap = case fScope frame of
                    ScopeMethod  _ meth -> methodLMap pid meth
                    ScopeProcess _ proc -> procLMap proc
                    ScopeTemplate       -> M.empty
    let ctx = CFACtx { ctxPID     = []
                     , ctxStack   = [(fScope frame, error "evalStr: return", Nothing, lmap)]
                     , ctxCFA     = error "evalStr: CFA undefined"
                     , ctxBrkLocs = []
                     , ctxGNMap   = globalNMap
                     , ctxLastVar = 0
                     , ctxVar     = []}
        iexpr = evalState (exprToIExprDet simpexpr) ctx
    -- 6. evaluate
    return $ storeEval store iexpr


-- Create a store with all tmp variables assigned to Nothing
emptyTmpStore :: SourceView c a -> Store
emptyTmpStore sv = 
    storeUnions $ map (\v -> SStruct [varName v, storeNonDet $ varType v])
                $ C.specTmpVars $ svSpec sv

-- Create a store with all tmp variables assigned according to their values 
-- in the transition
tranTmpStore :: SourceView c a -> D.Transition a Store -> Store
tranTmpStore sv tran = 
    storeUnions $ map (\v -> SStruct [(varName v, storeEval tstore (I.EVar $ I.varName v))])
                $ C.specTmpVars $ svSpec sv
    where tstore = D.tranConcreteLabel tran

-- Message boxes
showMessage :: RModel c a b -> G.MessageType -> String -> IO ()
showMessage ref mtype mtext = do
    dialog <- messageDialogNew Nothing [DialogModal] mtype ButtonsOk mtext
    onResponse dialog (\_ -> widgetDestroy dialog)
    windowPresent dialog
