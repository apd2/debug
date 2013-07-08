{-# LANGUAGE ImplicitParams, RecordWildCards, ScopedTypeVariables, TupleSections #-}

module SourceView(sourceViewNew, 
                  simulateTransition) where

import Data.Maybe
import Data.List
import Data.Tree
import Data.String.Utils
import qualified Data.Map                   as M
import qualified Data.Set                   as S
import Data.IORef
import Data.Hashable
import qualified Data.Graph.Inductive.Graph as Graph
import qualified Graphics.UI.Gtk            as G
import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import System.IO.Error
import Text.Parsec
import Control.Applicative
import qualified Text.PrettyPrint           as PP
import Debug.Trace

import Name
import Pos
import PP
import qualified Parse
import Util hiding (name, trace)
import TSLUtil
import qualified DbgTypes      as D
import qualified IDE           as D
import qualified DbgAbstract   as D
--import qualified DbgConcretise as D
import ISpec
import TranSpec
import IExpr
import IVar
import IType
import CFA
import Inline
import Predicate
import Store
import SMTSolver
import TSLAbsGame

import qualified NS              as Front
import qualified Method          as Front
import qualified Process         as Front
import qualified InstTree        as Front
import qualified TemplateOps     as Front
import qualified ExprInline      as Front
import qualified Spec            as Front
import qualified Expr            as Front
import qualified ExprFlatten     as Front
import qualified ExprOps         as Front
import qualified ExprValidate    as Front
import qualified Statement       as Front
import qualified StatementOps    as Front
import qualified StatementInline as Front

--------------------------------------------------------------
-- Constants
--------------------------------------------------------------

colorCont  = "#80c080"
colorUCont = "#8080c0"
fontSrc    = "Courier 10 Pitch"
--------------------------------------------------------------
-- Types
--------------------------------------------------------------

instance D.Vals Store

data ProcStackFrame = FrameCTask       {frScope::Front.Scope, frLoc::Loc} 
                    | FrameRegular     {frScope::Front.Scope, frLoc::Loc}
                    | FrameInteractive {frScope::Front.Scope, frLoc::Loc, frCFA::CFA}

isFrameInteractive (FrameInteractive _ _ _) = True
isFrameInteractive _                        = False

instance PP ProcStackFrame where
    pp f = (if' (isFrameInteractive f) (PP.text "(interactive)") PP.empty) PP.<+> (PP.text $ show $ frScope f) PP.<> PP.char ':' PP.<+> (PP.text $ show $ frLoc f)

type ProcStack = [ProcStackFrame]

instance PP ProcStack where
    pp stack = PP.vcat $ map pp stack

showStack :: ProcStack -> String
showStack = PP.render . pp

stackToProcStack :: Bool -> Stack -> ProcStack
stackToProcStack True ((Frame sc loc):frames) = FrameCTask sc loc : stackToProcStack False frames
stackToProcStack _    frames                  = map (\(Frame sc loc) -> FrameRegular sc loc) frames

data TraceEntry = TraceEntry {
    teStack :: ProcStack,
    teStore :: Store
}


instance PP TraceEntry where
    pp (TraceEntry stack _) = pp stack

type Trace = [TraceEntry]

instance PP Trace where
    pp tr = PP.vcat $ PP.punctuate (PP.char '\n') $ mapIdx (\e i -> pp i PP.<> PP.char ':' PP.$$ pp e) tr

showTrace :: Trace -> String
showTrace = PP.render . pp

data SourceView c a = SourceView {
    svModel          :: D.RModel c a Store,
    svSpec           :: Spec,
    svInputSpec      :: Front.Spec,
    svFlatSpec       :: Front.Spec,
    svAbsVars        :: M.Map String AbsVar,
    svState          :: D.State a Store,            -- current state set via view callback
    svTmp            :: Store,                      -- temporary variables store
    svTranPID        :: Maybe PID,                  -- set by transitionSelected, restricts enabled processes to PID

    -- Command buttons
    svStepButton     :: G.ToolButton,
    svRunButton      :: G.ToolButton,
    svMagExitButton  :: G.ToolButton,               -- exit magic block

    -- Trace
    svTrace          :: Trace,                      -- steps from the current state
    svTracePos       :: Int,                        -- current position in the trace
    svTraceCombo     :: G.ComboBox,                 -- trace combo box
    svTraceStore     :: G.ListStore Int,            -- indices in the trace that correspond to visible locations
    svTraceUndo      :: G.ToolButton,               -- back button
    svTraceRedo      :: G.ToolButton,               -- forward button

    -- Process selector
    svPID            :: PID,                        -- PID set in the process selection menu
    svProcessCombo   :: G.ComboBox,                 -- process selection combo box
    svProcessStore   :: G.TreeStore (PID, Bool),    -- tree store that backs the process selector

    -- Stack view
    svStackView      :: G.TreeView,                 -- stack view
    svStackStore     :: G.ListStore ProcStackFrame, -- store containing list of stack frames
    svStackFrame     :: Int,                        -- selected stack frame (0 = top)

    -- Watch
    svWatchView      :: G.TreeView,
    svWatchStore     :: G.ListStore (Maybe String), -- store containing watch expressions

    -- Source window
    svSourceView     :: G.TextView,
    svSourceBuf      :: G.TextBuffer,
    svSourceTag      :: G.TextTag,                  -- tag to mark current selection current selection
    svFileLab        :: G.Label,                    -- Status labels: file name
    --svContTog        :: G.ToggleButton,             --                controllable/uncontrollable state
    --svContLab        :: G.Label,                    --
    svInprogLab      :: G.Label,                    --                transition in progress

    -- Resolve view
    svResolveStore   :: G.TreeStore Expr,           -- tmp variables in the scope of the current expression
    svAutoResolve    :: Bool,                       -- resolve non-determinism automatically
    svAutoResolveTog :: G.CheckButton,              -- toggle auto-resolve mode button

    -- Action selector
    svActSelectText  :: G.TextView,                 -- user-edited controllable action
    svActSelectBAdd  :: G.Button                    -- perform controllable action button
}

type RSourceView c a = IORef (SourceView c a)

sourceViewEmpty = SourceView { svModel          = error "SourceView: svModel undefined"
                             , svInputSpec      = error "SourceView: svInputSpec undefined"
                             , svFlatSpec       = error "SourceView: svFlatSpec undefined"
                             , svSpec           = error "SourceView: svSpec undefined" 
                             , svAbsVars        = error "SourceView: svAbsVars undefined"
                             , svState          = error "SourceView: svState undefined"
                             , svTmp            = error "SourceView: svTmp undefined"
                             , svTranPID        = Nothing
                             , svStepButton     = error "SourceView: svStepButton undefined"
                             , svRunButton      = error "SourceView: svRunButton undefined"
                             , svMagExitButton  = error "SourceView: svMagExitButton undefined"
                             , svTrace          = []
                             , svTracePos       = 0
                             , svTraceCombo     = error "SourceView: svTraceCombo undefined"
                             , svTraceStore     = error "SourceView: svTraceStore undefined"
                             , svTraceUndo      = error "SourceView: svTraceUndo undefined"
                             , svTraceRedo      = error "SourceView: svTraceRedo undefined"
                             , svPID            = error "SourceView: PID undefined"
                             , svProcessCombo   = error "SourceView: svProcessCombo undefined"
                             , svProcessStore   = error "SourceView: svProcessStore undefined"
                             , svStackView      = error "SourceView: svStackView undefined"
                             , svStackStore     = error "SourceView: svStackStore undefined"
                             , svStackFrame     = error "SourceView: svStackFrame undefined"
                             , svWatchView      = error "SourceView: scWatchView undefined"
                             , svWatchStore     = error "SourceView: svWatchStore undefined"
                             , svSourceView     = error "SourceView: svSourceView undefined"
                             , svSourceBuf      = error "SourceView: svSourceBuf undefined"
                             , svSourceTag      = error "SourceView: svSourceTag undefined"
                             , svFileLab        = error "SourceView: svFileLab undefined"
                             , svInprogLab      = error "SourceView: svInprogLab undefined"
                             , svResolveStore   = error "SourceView: svResolveStore undefined"
                             , svAutoResolve    = True
                             , svAutoResolveTog = error "SourceView: svAutoResolveTog undefined"
                             , svActSelectText  = error "SourceView: svActSelectText undefined"
                             , svActSelectBAdd  = error "SourceView: svActSelectBAdd undefined"
                             }


--------------------------------------------------------------
-- View callbacks
--------------------------------------------------------------

sourceViewNew :: (D.Rel c v a s) => Front.Spec -> Front.Spec -> Spec -> M.Map String AbsVar -> SMTSolver -> D.RModel c a Store -> IO (D.View a Store)
sourceViewNew inspec flatspec spec absvars solver rmodel = do

    ref <- newIORef $ sourceViewEmpty { svModel          = rmodel
                                      , svInputSpec      = inspec
                                      , svFlatSpec       = flatspec
                                      , svSpec           = specInlineWireAlways spec
                                      , svAbsVars        = absvars
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
    G.containerAdd selitem sel
    G.toolbarInsert tbar selitem (-1)

    sep1 <- G.separatorToolItemNew
    G.widgetShow sep1
    G.toolbarInsert tbar sep1 (-1)

    -- command buttons
    butstep <- G.toolButtonNewFromStock G.stockGoForward
    G.set butstep [G.widgetTooltipText G.:= Just "step"]
    _ <- G.onToolButtonClicked butstep (stepAction ref)
    G.widgetShow butstep
    G.toolbarInsert tbar butstep (-1)

    butrun <- G.toolButtonNewFromStock G.stockGotoLast
    G.set butrun [G.widgetTooltipText G.:= Just "run"]
    _ <- G.onToolButtonClicked butrun (runAction ref)
    G.widgetShow butrun
    G.toolbarInsert tbar butrun (-1)

    bexit <- G.toolButtonNewFromStock G.stockClose
    G.set bexit [G.widgetTooltipText G.:= Just "Exit Magic Block"]
    _ <- G.onToolButtonClicked bexit (exitMagicBlock ref)
    G.widgetShow bexit
    G.toolbarInsert tbar bexit (-1)    

    modifyIORef ref (\sv -> sv { svRunButton     = butrun
                               , svStepButton    = butstep
                               , svMagExitButton = bexit})

    sep2 <- G.separatorToolItemNew
    G.widgetShow sep2
    G.toolbarInsert tbar sep2 (-1)

    -- trace
    tr <- traceViewCreate ref
    tritem <- G.toolItemNew
    G.widgetShow tritem
    G.containerAdd tritem tr
    G.toolbarInsert tbar tritem (-1)

    -- horizontal PanedPanels
    hpanes <- D.panedPanelsNew (liftM G.toPaned $ G.hPanedNew)
    w <- D.panelsGetWidget hpanes
    G.boxPackStart vbox w G.PackGrow 0

    -- source window and controllable action selector on the left
    lvpanes  <- D.panedPanelsNew (liftM G.toPaned $ G.vPanedNew)
    wlvpanes <- D.panelsGetWidget lvpanes
    D.panelsAppend hpanes wlvpanes ""

    src <- sourceWindowCreate ref
    D.panelsAppend lvpanes src "Source Code"

    act <- actionSelectorCreate ref
    D.panelsAppend lvpanes act "Controllable action"

    -- stack, watch, resolve on the right
    rvpanes <- D.panedPanelsNew (liftM G.toPaned $ G.vPanedNew)
    wrvpanes <- D.panelsGetWidget rvpanes
    D.panelsAppend hpanes wrvpanes ""
    stack <- stackViewCreate ref
    watch <- watchCreate ref
    resolve <- resolveViewCreate ref
    D.panelsAppend rvpanes stack "Stack"
    D.panelsAppend rvpanes watch "Watch"
    D.panelsAppend rvpanes resolve "Resolve non-determinism"

    let cb = D.ViewEvents { D.evtStateSelected      = sourceViewStateSelected      ref 
                          , D.evtTransitionSelected = sourceViewTransitionSelected ref
                          , D.evtTRelUpdated        = return ()
                          }

    -- disable everything until we get a state to start from
    disable ref

    -- HACK: create an initial state
    model <- readIORef rmodel
    when (isNothing $ find ((== "init") . fst) $ D.mStateRels model) $ do
        let ?spec    = spec
            ?absvars = absvars
            ?model   = model
            ?m       = D.mCtx model
        initstore <- liftM storeExtendDefault
                     $ case smtGetModel solver [let ?pred = [] in bexprToFormula $ snd $ tsInit $ specTran spec] of
                            Just (Right store) -> return store                      
                            _                  -> fail "Unsatisfiable initial condition"
        -- more hack: simulate init transition
        modifyIORef ref (\sv -> sv { svState      = D.abstractState initstore
                                   , svTmp        = SStruct $ M.empty
                                   , svPID        = ["$init"]
                                   , svTrace      = [TraceEntry { teStack = [FrameRegular (Front.ScopeTemplate (let ?spec = flatspec in tmMain)) (tranFrom $ fst $ tsInit $ specTran ?spec)]
                                                                , teStore = initstore}]
                                   , svTracePos   = 0
                                   , svStackFrame = 0})

        runAction ref
        initstore' <- getIORef currentStore ref
        D.modelSelectState rmodel (Just $ D.abstractState initstore') 
    -- END HACK

    return $ D.View { D.viewName      = "Source"
                    , D.viewDefAlign  = D.AlignCenter
                    , D.viewShow      = return ()
                    , D.viewHide      = return ()
                    , D.viewGetWidget = return $ G.toWidget vbox
                    , D.viewCB        = cb
                    }
    

sourceViewStateSelected :: (D.Rel c v a s) => RSourceView c a -> Maybe (D.State a Store) -> IO ()
sourceViewStateSelected ref Nothing                                = disable ref
sourceViewStateSelected ref (Just s) | (not $ D.isConcreteState s) = disable ref
                                     | otherwise                   = do
    putStrLn $ "sourceViewStateSelected: store: " ++ (show $ D.sConcrete s)
    modifyIORef ref (\sv -> sv { svState   = s
                               , svTmp     = SStruct $ M.empty
                               , svTranPID = Nothing})
    processSelectorChooseUniqueEnabled ref
    reset ref
    putStrLn "sourceViewStateSelected done"

sourceViewTransitionSelected :: (D.Rel c v a s) => RSourceView c a -> D.Transition a Store -> IO ()
sourceViewTransitionSelected ref tran | (not $ D.isConcreteTransition tran) = disable ref
                                      | otherwise                           = do
    putStrLn "sourceViewTransitionSelected"
    modifyIORef ref (\sv -> sv { svState   = D.tranFrom tran
                               , svTmp     = fromJust $ D.tranConcreteLabel tran
                               , svTranPID = Just $ parsePIDEnumerator $ storeEvalEnum (fromJust $ D.sConcrete $ D.tranTo tran) mkPIDVar})
    processSelectorChooseUniqueEnabled ref
    reset ref
    putStrLn "sourceViewTransitionSelected done"

--------------------------------------------------------------
-- Actions
--------------------------------------------------------------

stepAction :: (D.Rel c v a s) => RSourceView c a -> IO ()
stepAction ref = do 
    sv   <- readIORef ref
    -- sync PID
    let sv0 = maybeSetLCont $ setLPID (svPID sv) sv
    let msv' = step sv0
    when (isJust msv') $ do writeIORef ref (fromJust msv')
                            when (currentDelay $ fromJust msv') $ makeTransition ref
                            updateDisplays ref

-- Execute one statement
step :: SourceView c a -> Maybe (SourceView c a)
step sv = 
    let sv0 = sv {svStackFrame = 0} in
    case microstep sv0 of
         Just sv' -> let -- did we reach the next statement?
                         lab    = currentLocLabel sv'
                         action = locAct lab
                     in if isActNone action && (not $ isDelayLabel lab)
                           then step sv'
                           else Just sv'
         Nothing -> Nothing


runAction :: (D.Rel c v a s) => RSourceView c a -> IO ()
runAction ref = do
    sv <- readIORef ref
     -- sync PID
    let sv0 = maybeSetLCont $ setLPID (svPID sv) sv
    case run sv0 of
         Nothing  -> return ()
         Just sv' -> do writeIORef ref sv'
                        when (currentDelay sv') $ makeTransition ref
                        updateDisplays ref
    

-- run until pause or nondeterministic choice
run :: SourceView c a -> Maybe (SourceView c a)
run sv = case step sv of
              Nothing  -> Nothing
              Just sv' -> if currentDelay sv' 
                             then Just sv'
                             else case run sv' of
                                       Nothing   -> Just sv'
                                       Just sv'' -> Just sv''

exitMagicBlock :: (D.Rel c v a s) => RSourceView c a -> IO ()
exitMagicBlock ref = do
    switchToControllable ref
    modifyIORef ref (\sv -> setPID pidCont 
                            $ modifyCurrentStore sv (\st0 -> let st1 = storeSet st0 mkMagicVar (Just $ SVal $ BoolVal False)
                                                                 st2 = storeSet st1 mkContVar  (Just $ SVal $ BoolVal False)
                                                             in st2))
    makeTransition ref

-- simulate transition without GUI 
simulateTransition :: Front.Spec -> Spec -> M.Map String AbsVar -> Store -> Store -> Maybe Store
simulateTransition flatspec spec absvars st lab =
    let -- create enough of source view to call run
        pid = parsePIDEnumerator $ storeEvalEnum lab mkPIDLVar
        sv0 :: SourceView () ()
        sv0 = sourceViewEmpty { svSpec       = spec
                              , svFlatSpec   = flatspec
                              , svAbsVars    = absvars
                              , svState      = D.State { sAbstract = error "simulateTransition: sAbstract is undefined"
                                                       , sConcrete = Just st}
                              , svTmp        = lab
                              , svTranPID    = Just pid
                              , svTracePos   = 0
                              , svPID        = pid
                              , svStackFrame = 0
                              }
        sv1 = if storeEvalBool st (EVar mkContVarName)
                 then -- execute controllable CFA
                      sv0 {svTrace = [TraceEntry { teStore = storeUnion st lab
                                                 , teStack = [FrameInteractive Front.ScopeTop cfaInitLoc (specCAct spec)]}]}
                 else -- execute uncontrollable process from its current location
                      sv0 {svTrace = [TraceEntry { teStore = storeUnion st lab 
                                                 , teStack = stackFromStore sv0 st pid}]} 
        msv2 = run sv1 
        mstore2 = case msv2 of
                       Nothing  -> Nothing
                       Just sv2 -> if not $ currentDelay sv2
                                      then Nothing
                                      else Just $ currentStore sv2
    in trace ("simulateTransition\nlabel: " ++ show lab ++ "\nstate: " ++ show st)
       $ mstore2

--------------------------------------------------------------
-- GUI components
--------------------------------------------------------------

-- Process selector --
processSelectorCreate :: RSourceView c a -> IO G.Widget
processSelectorCreate ref = do
    sv <- readIORef ref
    hbox <- G.hBoxNew False 0
    G.widgetShow hbox
    lab <- G.labelNew $ Just "Select process: "
    G.widgetShow lab
    G.boxPackStart hbox lab G.PackNatural 0
    combo <- G.comboBoxNew
    G.widgetShow combo
    G.boxPackStart hbox combo G.PackNatural 0
    store <- G.treeStoreNew []
    G.comboBoxSetModel combo (Just store)
    rend <- G.cellRendererTextNew
    G.cellLayoutPackStart combo rend True
    G.cellLayoutSetAttributeFunc combo rend store $ 
        (\iter -> do path <- G.treeModelGetPath store iter
                     (pid, en) <- G.treeStoreGetValue store path
                     G.set rend [G.cellTextMarkup G.:= Just $ "<span weight=\"" ++ (if en then "bold" else "normal") ++ "\">" ++ last pid ++ "</span>"])
    writeIORef ref sv {svProcessCombo = combo, svProcessStore = store}
    _ <- G.on combo G.changed (processSelectorChanged ref)
    processSelectorInit ref
    return $ G.toWidget hbox

processSelectorChanged :: RSourceView c a -> IO ()
processSelectorChanged ref = do
    sv <- readIORef ref
    miter <- G.comboBoxGetActiveIter $ svProcessCombo sv
    when (isJust miter) $ 
        do path <- G.treeModelGetPath (svProcessStore sv) (fromJust miter)
           (pid, _) <- G.treeStoreGetValue (svProcessStore sv) path
           putStrLn $ "setting PID to " ++ show pid
           modifyIORef ref (\_sv -> _sv{svPID = pid})
           --cfaShow (specGetCFA (svSpec sv) pid Nothing) (pidToName pid)
           reset ref


pidtree :: SourceView c a -> Forest PID 
pidtree sv = map (procTree []) (specProc $ svSpec sv)
    where procTree parpid p = Node { rootLabel = pid
                                   , subForest = map (procTree pid) (procChildren p)}
                              where pid = parpid ++ [procName p]

processSelectorInit :: RSourceView c a -> IO ()
processSelectorInit ref = do
    sv <- readIORef ref
    let store = svProcessStore sv

    -- build store
    G.treeStoreClear store
    G.treeStoreInsertForest store [] 0 (map (fmap (,False)) $ pidtree sv)

processSelectorUpdate :: RSourceView c a -> IO ()
processSelectorUpdate ref = do
    sv <- readIORef ref
    let store = svProcessStore sv
        combo = svProcessCombo sv
    myTreeModelForeach store (\iter -> do sv'      <- readIORef ref
                                          path     <- G.treeModelGetPath store iter
                                          (pid, _) <- G.treeStoreGetValue store path
                                          let en = isProcEnabled sv' pid
                                          G.treeStoreSetValue store path (pid, en))
    miter <- G.comboBoxGetActiveIter combo
    when (isNothing miter) $ do miter' <- G.treeModelGetIter store [0]
                                G.comboBoxSetActiveIter combo (fromJust miter')
    G.widgetSetSensitive combo True

-- Check if there's only one enabled process in the current state
-- and, if yes, select this process.
processSelectorChooseUniqueEnabled :: RSourceView c a -> IO ()
processSelectorChooseUniqueEnabled ref = do
    sv <- readIORef ref
    let enpids = filter (isProcEnabled sv)
                 $ concatMap flatten $ pidtree sv
    when (length enpids == 1) $ processSelectorSelectPID ref (head enpids)

processSelectorSelectPID :: RSourceView c a -> PID -> IO ()
processSelectorSelectPID ref pid = do
    sv <- readIORef ref
    let store = svProcessStore sv
        combo = svProcessCombo sv
    myTreeModelForeach store (\iter -> do path     <- G.treeModelGetPath store iter
                                          (pid', _) <- G.treeStoreGetValue store path
                                          when (pid' == pid) $ G.comboBoxSetActiveIter combo iter)

-- GTK's treeModelForeach does not work if the 
-- function modifies the store
myTreeModelForeach :: G.TreeModelClass self => self -> (G.TreeIter -> IO ()) -> IO ()
myTreeModelForeach m f = myTreeModelForeach' m Nothing f

myTreeModelForeach' :: G.TreeModelClass self => self -> Maybe G.TreeIter -> (G.TreeIter -> IO ()) -> IO ()
myTreeModelForeach' m miter f = do
    nchildren <- G.treeModelIterNChildren m miter
    path <- case miter of
                  Nothing -> return []
                  Just i  -> G.treeModelGetPath m i
    _ <- mapM (\i -> do miter' <- G.treeModelGetIter m path
                        child <- liftM fromJust $ G.treeModelIterNthChild m miter' i
                        cpath <- G.treeModelGetPath m child
                        f child
                        mchild' <- G.treeModelGetIter m cpath
                        myTreeModelForeach' m mchild' f) 
         $ [0..nchildren - 1]
    return ()


processSelectorDisable :: RSourceView c a -> IO ()
processSelectorDisable ref = do
    combo <- getIORef svProcessCombo ref
    G.widgetSetSensitive combo False

-- Stack --
stackViewCreate :: RSourceView c a -> IO G.Widget
stackViewCreate ref = do
    view <- G.treeViewNew
    G.treeViewSetHeadersVisible view False
    G.widgetShow view
    store <- G.listStoreNew []

    col <- G.treeViewColumnNew

    rend <- G.cellRendererTextNew
    G.cellLayoutPackStart col rend True
    G.cellLayoutSetAttributeFunc col rend store $ 
        (\iter -> do let idx = G.listStoreIterToIndex iter
                     frame <- G.listStoreGetValue store idx
                     G.set rend [G.cellTextMarkup G.:= Just $ show $ frScope frame])
    _ <- G.treeViewAppendColumn view col

    modifyIORef ref (\sv -> sv{svStackView = view, svStackStore = store, svStackFrame = 0})
    G.treeViewSetModel view store
    _ <- G.on view G.rowActivated (stackViewFrameSelected ref)
    panel <- D.framePanelNew (G.toWidget view) "Stack" (return ())
    D.panelGetWidget panel

stackViewFrameSelected :: RSourceView c a -> G.TreePath -> G.TreeViewColumn -> IO ()
stackViewFrameSelected ref (idx:_) _ = do
    modifyIORef ref (\sv -> sv{svStackFrame = idx})
    sourceWindowUpdate ref
    watchUpdate ref

stackViewUpdate :: RSourceView c a -> IO ()
stackViewUpdate ref = do
    sv <- readIORef ref
    let store = svStackStore sv
    G.listStoreClear store
    _ <- mapM (G.listStoreAppend store) $ currentStack sv
    return ()

stackViewDisable :: RSourceView c a -> IO ()
stackViewDisable ref = do
    sv <- readIORef ref
    G.listStoreClear $ svStackStore sv

-- Trace --

-- indices of visible states in the trace
svTraceVisible :: SourceView c a -> [Int]
svTraceVisible sv = 
    filter (\i -> case locAct (getLocLabel sv i) of
                       ActNone -> False
                       _       -> True)
    $ [0..length (svTrace sv) - 1]

traceViewCreate :: RSourceView c a -> IO G.Widget
traceViewCreate ref = do
    hbox <- G.hBoxNew False 0
    G.widgetShow hbox

    -- undo button
    undo <- G.toolButtonNewFromStock G.stockUndo
    G.widgetShow undo
    G.boxPackStart hbox undo G.PackNatural 0
    _ <- G.onToolButtonClicked undo (do modifyIORef ref (\sv -> traceSetPos sv $ last $ filter (< svTracePos sv) $ svTraceVisible sv)
                                        updateDisplays ref)

    -- trace
    combo <- G.comboBoxNew
    G.widgetShow combo
    G.boxPackStart hbox combo G.PackNatural 0
    store <- G.listStoreNew []
    G.comboBoxSetModel combo (Just store)    
    rend <- G.cellRendererTextNew
    G.cellLayoutPackStart combo rend True
    G.cellLayoutSetAttributeFunc combo rend store $ 
        (\iter -> do let storeidx = G.listStoreIterToIndex iter
                     idx  <- G.listStoreGetValue store storeidx
                     sv   <- readIORef ref
                     let txt = take 48 $ show $ replace "\n" " " $
                               case locAct $ getLocLabel sv idx of
                                    ActStat s -> show s
                                    ActExpr e -> show e
                                    _         -> "?"
                     G.set rend [G.cellTextMarkup G.:= Just $ "<span weight=\"" ++ (if idx == svTracePos sv then "bold" else "normal") ++ "\">" ++ txt ++ "</span>"])
    _ <- G.on combo G.changed (tracePosChanged ref)

    -- redo button
    redo <- G.toolButtonNewFromStock G.stockRedo
    G.widgetShow redo
    G.boxPackStart hbox redo G.PackNatural 0
    _ <- G.onToolButtonClicked redo (do modifyIORef ref (\sv -> traceSetPos sv $ head $ filter (> svTracePos sv) $ svTraceVisible sv)
                                        updateDisplays ref)

    modifyIORef ref $ (\sv -> sv { svTraceStore = store
                                 , svTraceCombo = combo
                                 , svTraceUndo  = undo
                                 , svTraceRedo  = redo})

    return $ G.toWidget hbox

tracePosChanged :: RSourceView c a -> IO ()
tracePosChanged ref = do
    sv <- readIORef ref
    miter <- G.comboBoxGetActiveIter $ svTraceCombo sv
    when (isJust miter) $ 
        do let storeidx = G.listStoreIterToIndex $ fromJust miter
           p <- G.listStoreGetValue (svTraceStore sv) storeidx
           -- only call updateDisplays if this is not a recursive call from updateDisplays
           when (p /= svTracePos sv) $ do writeIORef ref (traceSetPos sv p)
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
    putStrLn $ "traceViewUpdate:\n" ++ (showTrace $ svTrace sv)
    -- update store
    G.listStoreClear $ svTraceStore sv
    _ <- mapM (G.listStoreAppend (svTraceStore sv)) $ svTraceVisible sv

    -- set selection
    items <- G.listStoreToList (svTraceStore sv)
    case findIndex (svTracePos sv ==) items of
         Nothing -> G.comboBoxSetActive (svTraceCombo sv) (-1)
         Just i  -> G.comboBoxSetActive (svTraceCombo sv) i

    -- enable/disable buttons
    G.widgetSetSensitive (svTraceUndo sv) (svTracePos sv /= 0)
    G.widgetSetSensitive (svTraceRedo sv) (svTracePos sv /= length (svTrace sv) - 1)
    G.widgetSetSensitive (svTraceCombo sv) True


traceAppend :: SourceView c a -> Store -> ProcStack -> SourceView c a
traceAppend sv store stack = sv {svTrace = tr, svTracePos = p}
    where tr = take (svTracePos sv + 1) (svTrace sv) ++ [TraceEntry stack store]
          p  = length tr - 1

traceSetPos :: SourceView c a -> Int -> SourceView c a
traceSetPos sv i | (i >= (length $ svTrace sv)) || (i < 0) = sv
                 | otherwise = sv {svTracePos = i, svStackFrame = 0}


-- Watch --

watchCreate :: RSourceView c a -> IO G.Widget
watchCreate ref = do
    view <- G.treeViewNew
    G.widgetShow view

    store <- G.listStoreNew [Nothing]

    namecol <- G.treeViewColumnNew
    G.treeViewColumnSetTitle namecol "Watch expression"
    G.treeViewColumnSetResizable namecol True

    exprend <- G.cellRendererTextNew
    G.cellLayoutPackStart namecol exprend False
    G.cellLayoutSetAttributeFunc namecol exprend store $ 
        (\iter -> do let idx = G.listStoreIterToIndex iter
                     mexp <- G.listStoreGetValue store idx
                     G.set exprend [ G.cellTextEditable G.:= True
                                   , G.cellSensitive    G.:= True
                                   , G.cellTextMarkup   G.:= Just 
                                                             $ case mexp of 
                                                                    Nothing -> "<i>Add watch</i>"
                                                                    Just e  -> e])
    _ <- G.on exprend G.edited (watchChanged ref)
    _ <- G.on exprend G.editingStarted (watchEditingStarted ref)
    _ <- G.treeViewAppendColumn view namecol

    valcol <- G.treeViewColumnNew
    G.treeViewColumnSetTitle valcol "Value"
    G.treeViewColumnSetResizable valcol True

    valrend <- G.cellRendererTextNew
    G.cellLayoutPackStart valcol valrend True
    G.cellLayoutSetAttributeFunc valcol valrend store $ 
        (\iter -> do sv@SourceView{..} <- readIORef ref
                     let idx = G.listStoreIterToIndex iter
                     mexp <- G.listStoreGetValue store idx
                     G.set valrend [G.cellTextMarkup G.:= Just 
                                                          $ case mexp of 
                                                                 Nothing  -> ""
                                                                 Just e -> case storeEvalStr svInputSpec svFlatSpec  
                                                                                             (currentStore sv) svPID (frScope $ currentStack sv !! svStackFrame) e of
                                                                                Left er -> er
                                                                                Right v -> show v])
    _ <- G.treeViewAppendColumn view valcol

    _ <- G.on view G.keyPressEvent (do key <- G.eventKeyVal
                                       when (key == G.keyFromName "Delete") $ liftIO $ watchDelete ref
                                       when (isJust $ G.keyToChar key)      $ liftIO $ (do (path, _) <- G.treeViewGetCursor view
                                                                                           G.treeViewSetCursor view path (Just $ (namecol, True)))
                                       return True)
    modifyIORef ref (\sv -> sv{svWatchView = view, svWatchStore = store})

    G.treeViewSetModel view store
    panel <- D.framePanelNew (G.toWidget view) "Watch" (return ())
    D.panelGetWidget panel

watchEditingStarted :: RSourceView c a ->  G.Widget -> G.TreePath -> IO ()
watchEditingStarted ref w path = do
    let entry = G.castToEntry w
    store <- getIORef svWatchStore ref
    val <- G.listStoreGetValue store (head path)
    when (isNothing val) $ G.entrySetText entry ""

watchChanged :: RSourceView c a -> G.TreePath -> String -> IO ()
watchChanged ref path val = do
    store <- getIORef svWatchStore ref
    G.listStoreSetValue store (head path) (Just val)
    watchUpdate ref 

watchDelete :: RSourceView c a -> IO ()
watchDelete ref = do
    sv <- readIORef ref
    (idx, _) <- G.treeViewGetCursor (svWatchView sv)
    when (not $ null idx) $ G.listStoreRemove (svWatchStore sv) (head idx)
    watchUpdate ref

watchUpdate :: RSourceView c a -> IO ()
watchUpdate ref = do
    store <- getIORef svWatchStore ref
    items <- G.listStoreToList store
    -- make sure that there is an empty slot in the end
    let items' = (filter isJust items) ++ [Nothing]
    -- refill the list to force watch update
    G.listStoreClear store
    _ <- mapM (G.listStoreAppend store) items'
    return ()

watchDisable :: RSourceView c a -> IO ()
watchDisable _ = return ()


-- Source --
sourceWindowCreate :: (D.Rel c v a s) => RSourceView c a -> IO G.Widget
sourceWindowCreate ref = do
    vbox <- G.vBoxNew False 0
    G.widgetShow vbox
    -- Source widow at the top
    scroll <- G.scrolledWindowNew Nothing Nothing
    G.widgetShow scroll
    G.boxPackStart vbox scroll G.PackGrow 0

    view <- G.textViewNew
    font <- G.fontDescriptionFromString fontSrc
    G.widgetModifyFont view $ Just font
    G.widgetSetSizeRequest view 600 300
    G.widgetShow view
    G.containerAdd scroll view
    G.textViewSetEditable view False
    tag <- G.textTagNew Nothing
    buf <- G.textBufferNew Nothing
    table <- G.textBufferGetTagTable buf
    G.textTagTableAdd table tag
    G.textViewSetBuffer view buf
    -- Status bar at the bottom
    sbar <- G.hBoxNew True 0
    G.widgetShow sbar
    G.boxPackStart vbox sbar G.PackNatural 0
    -- file name label
    lfile <- G.labelNew Nothing
    G.widgetShow lfile
    G.boxPackStart sbar lfile G.PackGrow 0
--    -- cont/ucont switcher
--    bcont <- G.toggleButtonNew
--    G.widgetShow bcont
--    G.boxPackStart sbar bcont G.PackGrow 0
--    lcont <- G.labelNew Nothing
--    G.widgetShow lcont
--    G.containerAdd bcont lcont
--    _ <- G.on bcont G.toggled (contToggled ref)
    -- transition-in-progress label
    lprog <- G.labelNew Nothing
    G.widgetShow lprog
    G.boxPackStart sbar lprog G.PackGrow 0
   
    modifyIORef ref (\sv -> sv { svSourceView = view
                               , svSourceBuf  = buf
                               , svSourceTag  = tag
                               , svFileLab    = lfile
                               --, svContTog    = bcont
                               --, svContLab    = lcont
                               , svInprogLab  = lprog})
    return $ G.toWidget vbox

--contToggled :: (D.Rel c v a s) => RSourceView c a -> IO ()
--contToggled ref = do
--    putStrLn "contToggled"
--    sv <- readIORef ref
--    c <- G.toggleButtonGetActive (svContTog sv)
--    when (c && (not $ currentControllable sv)) $ switchToControllable ref

sourceWindowUpdate :: RSourceView c a -> IO ()
sourceWindowUpdate ref = do
    sv <- readIORef ref
    let cfa = cfaAtFrame sv (svStackFrame sv)
        loc = frLoc $ (currentStack sv) !! (svStackFrame sv)
    case locAct $ cfaLocLabel loc cfa of
         ActNone   -> do sourceClearPos sv
                         G.labelSetText (svFileLab sv) ""
         ActExpr e -> do sourceSetPos sv (pos e) colorUCont
                         G.labelSetText (svFileLab sv) (sourceName $ fst $ pos e)
         ActStat (Front.SMagic p mp _) -> do let col = if' (currentControllable sv) colorCont colorUCont
                                             if currentMagic sv 
                                                then sourceSetPos sv mp col
                                                else sourceSetPos sv p  col
                                             G.labelSetText (svFileLab sv) (sourceName $ fst p)
         ActStat s -> do sourceSetPos sv (pos s) colorUCont
                         G.labelSetText (svFileLab sv) (sourceName $ fst $ pos s)
--    if currentControllable sv
--       then do G.toggleButtonSetActive (svContTog sv) True
--               G.labelSetMarkup (svContLab sv) "<span color=\"green\"> CONTROLLABLE </span>"
--       else do G.toggleButtonSetActive (svContTog sv) False
--               G.labelSetMarkup (svContLab sv) "<span color=\"blue\">  UNCONTROLLABLE </span>"

    G.labelSetMarkup (svInprogLab sv) $ if svTracePos sv == 0
                                           then if currentError sv
                                                   then "<span color=\"red\" weight=\"bold\">ERROR</span>"
                                                   else "<span weight=\"bold\">PAUSE</span>" 
                                           else ""

--    G.widgetSetSensitive (svContTog sv) $ (not $ currentControllable sv) && svTracePos sv == 0 && currentMagic sv

sourceWindowDisable :: RSourceView c a -> IO ()
sourceWindowDisable ref = do
    sv <- readIORef ref
    G.labelSetText (svFileLab sv)   ""
    G.labelSetText (svInprogLab sv) ""
    sourceClearPos sv
    --G.labelSetText       (svContLab sv)   ""
    --G.widgetSetSensitive (svContTog sv)   False

sourceClearPos :: SourceView c a -> IO ()
sourceClearPos sv = do
    putStrLn "sourceClearPos"
    let buf   = svSourceBuf sv
    do istart <- G.textBufferGetStartIter buf
       iend <- G.textBufferGetEndIter buf
       G.textBufferRemoveTag buf (svSourceTag sv) istart iend
    `catchIOError` (\_ -> return ())

sourceSetPos :: SourceView c a -> Pos -> String -> IO ()
sourceSetPos sv (from, to) color = do
    putStrLn $ "sourceSetPos " ++ show (from, to)
    let fname = sourceName from
        buf   = svSourceBuf sv
    do src <- readFile fname
       G.textBufferSetText buf src
       istart <- G.textBufferGetStartIter buf
       iend <- G.textBufferGetEndIter buf
       ifrom <- G.textBufferGetIterAtLineOffset buf (sourceLine from - 1) (sourceColumn from - 1)
       ito <- G.textBufferGetIterAtLineOffset buf (sourceLine to - 1) (sourceColumn to - 1)
       G.textBufferRemoveTag buf (svSourceTag sv) istart iend
       G.set (svSourceTag sv) [G.textTagBackground G.:= color]
       G.textBufferApplyTag buf (svSourceTag sv) ifrom ito
       mark <- G.textMarkNew Nothing True
       G.textBufferAddMark buf mark ifrom
       _ <- G.textViewScrollToMark (svSourceView sv) mark 0.4 Nothing
       return ()
    `catchIOError` (\_ -> return ())



-- Command buttons --
commandButtonsUpdate :: RSourceView c a -> IO ()
commandButtonsUpdate ref = do
    sv <- readIORef ref
    -- enable step and run buttons if we are not at a pause location or
    -- if we are in a pause location and the wait condition is true
    let ?spec = svSpec sv
    let lab = currentLocLabel sv
        pen = isProcEnabled sv (svPID sv)
        en = -- current process must be enabled and ...
             case lab of
                  LInst _      -> True
                  LPause _ _ c -> pen && (storeEvalBool (currentStore sv) c == True)
                  LFinal _ _   -> pen && (not $ null $ Graph.lsuc (currentCFA sv) (currentLoc sv))
             && 
             -- ... non-determinism must be resolved
             -- (all scalar tmp variables that affect the next transition must be assigned)
             (all isJust           
              $ map (storeTryEval (currentStore sv))
              $ filter isTypeScalar
              $ concatMap flatten 
              $ currentTmpExprTree sv)
    G.widgetSetSensitive (svStepButton sv)    en
    G.widgetSetSensitive (svRunButton sv)     en
    G.widgetSetSensitive (svMagExitButton sv) $  (currentMagic sv)                       -- we're inside a magic block
                                              && (svTracePos sv == 0)                    -- there is not transition in progress
                                              && (not $ currentControllable sv)          -- we're in an uncontrollable state
                                              && isInsideMagicBlock (currentLocLabel sv) -- current process is inside the MB

commandButtonsDisable :: RSourceView c a -> IO ()
commandButtonsDisable ref = do
    sv <- readIORef ref
    -- disable command buttons
    G.widgetSetSensitive (svStepButton sv)    False
    G.widgetSetSensitive (svRunButton sv)     False
    G.widgetSetSensitive (svMagExitButton sv) False


-- Resolve --

resolveViewCreate :: RSourceView c a -> IO G.Widget
resolveViewCreate ref = do
    spec <- getIORef svSpec ref
    let ?spec = spec

    vbox <- G.vBoxNew False 0
    G.widgetShow vbox

    -- autoresolve enable/disable switch
    bauto <- G.checkButtonNewWithLabel "Auto-resolve non-determinism"
    G.widgetShow bauto
    G.boxPackStart vbox bauto G.PackNatural 0
    _ <- G.on bauto G.toggled (toggleAutoResolve ref)

    view <- G.treeViewNew
    G.widgetShow view
    G.boxPackStart vbox view G.PackGrow 0
    store <- G.treeStoreNew []
    G.treeViewSetModel view store

    -- Variable name column
    namecol <- G.treeViewColumnNew
    G.treeViewColumnSetTitle namecol "Variables"
    G.treeViewColumnSetResizable namecol True

    namerend <- G.cellRendererTextNew
    G.cellLayoutPackStart namecol namerend False
    G.cellLayoutSetAttributeFunc namecol namerend store $ 
        (\iter -> do sv   <- readIORef ref
                     path <- G.treeModelGetPath store iter
                     e    <- G.treeStoreGetValue store path
                     let hl = isNothing $ storeTryEval (currentStore sv) e
                     G.set namerend [G.cellTextMarkup G.:= Just $ if' hl ("<span background=\"red\">" ++ show e ++ "</span>") (show e)])
    _ <- G.treeViewAppendColumn view namecol

    -- Variable assignment column
    valcol <- G.treeViewColumnNew
    G.treeViewColumnSetTitle valcol "Value"
    G.treeViewColumnSetResizable valcol True

    textrend <- G.cellRendererTextNew
    G.cellLayoutPackStart valcol textrend True
    G.cellLayoutSetAttributeFunc valcol textrend store $ 
        (\iter -> do sv   <- readIORef ref
                     path <- G.treeModelGetPath store iter
                     e    <- G.treeStoreGetValue store path
                     G.set textrend [ G.cellVisible      G.:= isTypeInt e
                                    , G.cellTextEditable G.:= True
                                    , G.cellText         G.:= case storeTryEval (currentStore sv) e of
                                                                   Nothing -> "*"
                                                                   Just v  -> show v])
    _ <- G.on textrend G.edited (textAsnChanged ref)

    combrend <- G.cellRendererComboNew
    G.cellLayoutPackStart valcol combrend True
    G.cellLayoutSetAttributeFunc valcol combrend store $ 
        (\iter -> do sv              <- readIORef ref
                     path            <- G.treeModelGetPath store iter
                     e               <- G.treeStoreGetValue store path
                     (tmodel, colid) <- comboTextModel $ typ e
                     G.set combrend [ G.cellVisible        G.:= isTypeScalar e && not (isTypeInt e)
                                    , G.cellComboTextModel G.:= (tmodel, colid)
                                    , G.cellTextEditable   G.:= True
                                    , G.cellComboHasEntry  G.:= False
                                    , G.cellText           G.:= case storeTryEval (currentStore sv) e of
                                                                     Nothing -> "*"
                                                                     Just v  -> show v])
    _ <- G.on combrend G.edited (textAsnChanged ref) 

    _ <- G.treeViewAppendColumn view valcol
    modifyIORef ref (\sv -> sv { svResolveStore   = store
                               , svAutoResolveTog = bauto})
    panel <- D.framePanelNew (G.toWidget vbox) "Resolve non-determinism" (return ())
    D.panelGetWidget panel

toggleAutoResolve :: RSourceView c a -> IO ()
toggleAutoResolve ref = do
    sv <- readIORef ref
    mode <- G.toggleButtonGetActive $ svAutoResolveTog sv
    writeIORef ref $ sv {svAutoResolve = mode}

comboTextModel :: (?spec::Spec) => Type -> IO (G.ListStore String, G.ColumnId String String)
comboTextModel Bool     = do store <- G.listStoreNew ["*", "true", "false"]
                             let column = G.makeColumnIdString 0
                             G.customStoreSetColumn store column id
                             return (store, column)
comboTextModel (Enum n) = do store <- G.listStoreNew ("*": (enumEnums $ getEnumeration n))
                             let column = G.makeColumnIdString 0
                             G.customStoreSetColumn store column id
                             return (store, column)
comboTextModel _        = do store <- G.listStoreNew []
                             let column = G.makeColumnIdString 0
                             return (store, column)

textAsnChanged :: RSourceView c a -> G.TreePath -> String -> IO ()
textAsnChanged ref path valstr = do
    sv  <- readIORef ref
    let ?spec = svSpec sv
    e   <- G.treeStoreGetValue (svResolveStore sv) path
    val <- if valstr == "*"
              then return Nothing
              else case parseVal (typ e) valstr of
                        Left er -> do D.showMessage (svModel sv) G.MessageError er
                                      return Nothing
                        Right v -> return $ Just $ SVal v
    writeIORef ref $ modifyCurrentStore sv (\store -> storeSet store e val)
    updateDisplays ref

resolveViewUpdate :: RSourceView c a -> IO ()
resolveViewUpdate ref = do
    sv <- readIORef ref
    G.toggleButtonSetActive (svAutoResolveTog sv) (svAutoResolve sv)
    let exprs = currentTmpExprTree sv
        store = svResolveStore sv
    G.treeStoreClear store
    G.treeStoreInsertForest store [] 0 exprs

resolveViewDisable :: RSourceView c a -> IO ()
resolveViewDisable ref = do
    sv <- readIORef ref
    G.treeStoreClear $ svResolveStore sv

autoResolve :: RSourceView c a -> IO ()
autoResolve ref = do
     sv <- readIORef ref
     let ?spec = svSpec sv
     _ <- mapM (autoResolve1 ref)
          $ filter isTypeScalar
          $ concatMap flatten 
          $ currentTmpExprTree sv
     return ()

autoResolve1 :: RSourceView c a -> Expr -> IO ()
autoResolve1 ref e = do
    modifyIORef ref (\sv -> let ?spec = svSpec sv in
                            if isNothing $ storeTryEval (currentStore sv) e
                               then modifyCurrentStore sv (\s -> storeSet s e (Just $ SVal $ valDefault e))
                               else sv)
    

-- Controllable action selector --

actionSelectorCreate :: (D.Rel c v a s) => RSourceView c a -> IO G.Widget
actionSelectorCreate ref = do
    vbox <- G.vBoxNew False 0
    G.widgetShow vbox

    -- Text view for specifying controllable action
    tview <- G.textViewNew
    G.textViewSetEditable tview True
    font <- G.fontDescriptionFromString fontSrc
    G.widgetModifyFont tview $ Just font
    G.widgetShow tview
    G.boxPackStart vbox tview G.PackGrow 0

    bbox <- G.hButtonBoxNew
    G.buttonBoxSetLayout bbox G.ButtonboxStart
    G.widgetShow bbox
    G.boxPackStart vbox bbox G.PackNatural 0

--    ---- Exit magic block button
--    bexit <- G.buttonNewWithLabel "Exit Magic Block"
--    G.widgetShow bexit
--    G.containerAdd bbox bexit
--    _ <- G.on bexit G.buttonActivated (actionSelectorExit ref)

    -- Add transition button
    badd <- G.buttonNewWithLabel "Perform controllable action"
    G.widgetShow badd
    G.containerAdd bbox badd
    _ <- G.on badd G.buttonActivated (actionSelectorRun ref)

    modifyIORef ref (\sv -> sv { svActSelectText  = tview
                               , svActSelectBAdd  = badd})
    actionSelectorDisable ref
    panel <- D.framePanelNew (G.toWidget vbox) "Controllable action" (return ())
    D.panelGetWidget panel

actionSelectorRun :: (D.Rel c v a s) => RSourceView c a -> IO ()
actionSelectorRun ref = do
    sv@SourceView{..} <- readIORef ref
    buf <- G.textViewGetBuffer svActSelectText
    text <- G.get buf G.textBufferText
    let fname = hash text
        fpath = "/tmp/" ++ show fname
    case compileControllableAction svInputSpec svFlatSpec svPID (frScope $ head $ currentStack sv) text fpath of
         Left e    -> D.showMessage svModel G.MessageError e
         Right cfa -> do switchToControllable ref 
                         writeFile fpath text
                         runControllableCFA ref cfa
    


--actionSelectorExit :: (D.Rel c v a s) => RSourceView c a -> IO ()
--actionSelectorExit ref = do
--    modifyIORef ref (\sv -> setPID pidCont 
--                            $ modifyCurrentStore sv (\st0 -> let st1 = storeSet st0 mkMagicVar (Just $ SVal $ BoolVal False)
--                                                                 st2 = storeSet st1 mkContVar  (Just $ SVal $ BoolVal False)
--                                                             in st2))
--    makeTransition ref

switchToControllable :: (D.Rel c v a s) => RSourceView c a -> IO ()
switchToControllable ref = do
    sv0 <- readIORef ref
    let sv1 = modifyCurrentStore sv0 (\st0 -> storeSet st0 mkContVar (Just $ SVal $ BoolVal True))
        sv2 = setPID pidIdle sv1
    writeIORef ref sv2
    makeTransition ref

runControllableCFA :: RSourceView c a -> CFA -> IO ()
runControllableCFA ref cfa = do
    -- Push controllable cfa on the stack
    modifyIORef ref (\sv -> let stack = currentStack sv
                                stack' = (FrameInteractive (frScope $ head stack) cfaInitLoc cfa) : stack
                            in traceAppend sv (currentStore sv) stack')
    updateDisplays ref

actionSelectorUpdate :: RSourceView c a -> IO ()
actionSelectorUpdate ref = do
    sv <- readIORef ref
    putStrLn $ "actionSelectorUpdate: controllable=" ++ 
               (show $ currentControllable sv) ++ 
               " waitformagic=" ++ 
               (show $ isWaitForMagicLabel $ currentLocLabel sv)
    if (isWaitForMagicLabel $ currentLocLabel sv)
       then actionSelectorEnable ref
       else actionSelectorDisable ref

actionSelectorDisable :: RSourceView c a -> IO ()
actionSelectorDisable ref = do
    SourceView{..} <- readIORef ref
    G.widgetSetSensitive svActSelectBAdd False

actionSelectorEnable :: RSourceView c a -> IO ()
actionSelectorEnable ref = do
    SourceView{..} <- readIORef ref
    G.widgetSetSensitive svActSelectBAdd  True


--------------------------------------------------------------
-- Private helpers
--------------------------------------------------------------

isWaitForMagicLabel :: LocLabel -> Bool
isWaitForMagicLabel (LPause _ _ e) = isWaitForMagic e
isWaitForMagicLabel _              = False

-- Given a snapshot of the store at a pause location, compute
-- process stack.
stackFromStore :: SourceView c a -> Store -> PID -> ProcStack
stackFromStore sv s pid = stackFromStore' sv s pid Nothing

stackFromStore' :: SourceView c a -> Store -> PID -> Maybe String -> ProcStack
stackFromStore' sv s pid methname = stack' ++ stack
    where cfa   = specGetCFA (svSpec sv) pid methname
          loc   = storeGetLoc s pid methname
          lab   = cfaLocLabel loc cfa
          stack = stackToProcStack (null pid) (locStack lab)
          -- If this location corresponds to a task call, recurse into task's CFA
          stack' = case lab of 
                        LPause _ _ e -> case isWaitForTask e of
                                             Nothing  -> if isWaitForMagic e
                                                            then let t = getTag s
                                                                 in if' (t==mkTagIdle) []
                                                                    $ stackFromStore' sv s [] (Just t)
                                                            else []
                                             Just nam -> let ?spec = svFlatSpec sv in
                                                         let meth = snd $ Front.getMethod (Front.ScopeTemplate tmMain) (Front.MethodRef nopos [Ident nopos nam]) in
                                                         if storeEvalBool s $ mkEnVar pid (Just meth)
                                                            then stackFromStore' sv s pid (Just nam)
                                                            else []
                        _            -> []

-- Extract the last controllable or uncontrollable task call 
-- from the stack or return Nothing if the stack does not 
-- contain a task call
--stackTask :: Stack -> Int -> Maybe Front.Method
--stackTask stack frame = stackTask' $ drop frame stack
--
--stackTask' :: Stack -> Maybe Front.Method
--stackTask' []      = Nothing
--stackTask' (fr:st) = case fScope fr of
--                          Front.ScopeMethod _ m -> if Front.methCat m == Front.Task Front.Uncontrollable 
--                                                      then Just m
--                                                      else stackTask' st
--                          _               -> stackTask' st

storeGetLoc :: Store -> PID -> Maybe String -> Loc
storeGetLoc s pid methname = pcEnumToLoc pc
    where pcvar = mkPCVar $ pid ++ maybeToList methname
          pc    = storeEvalEnum s pcvar

getTag :: Store -> String
getTag s = storeEvalEnum s mkTagVar

isProcEnabled :: SourceView c a -> PID -> Bool
isProcEnabled sv pid = 
    let store = fromJust $ D.sConcrete $ svState sv
        stack = stackFromStore sv store pid
        frame = head stack
        loc   = frLoc frame
        cfa   = stackGetCFA sv pid stack
        lab   = cfaLocLabel loc cfa
    in case svTranPID sv of
            Just pid' -> pid' == pid
            _         -> -- The process is always enabled if it is inside a magic block
                         -- Otherwise, the process is enabled if its
                         -- pause condition holds
                         if isInsideMagicBlock lab
                            then True
                            else case lab of
                                      LPause _ _ cond -> storeEvalBool store cond == True
                                      LFinal _ _      -> not $ null $ Graph.lsuc cfa loc
                                      _               -> True

isInsideMagicBlock :: LocLabel -> Bool
isInsideMagicBlock (LPause _ _ cond) = cond == mkMagicDoneCond
isInsideMagicBlock _                 = False

-- update all displays
updateDisplays :: RSourceView c a -> IO ()
updateDisplays ref = do
    autores <- getIORef svAutoResolve ref
    when autores $ autoResolve ref
    commandButtonsUpdate ref
    stackViewUpdate      ref
    traceViewUpdate      ref
    watchUpdate          ref
    sourceWindowUpdate   ref
    resolveViewUpdate    ref
    actionSelectorUpdate ref

-- Reset all components
reset :: RSourceView c a -> IO ()
reset ref = do
    -- processSelectorUpdate expects initialised store
    sv0 <- readIORef ref
    let store = storeUnion (fromJust $ D.sConcrete $ svState sv0) (svTmp sv0)
        tr = [TraceEntry { teStack = error "teStack is undefined"
                         , teStore = store }] 
    writeIORef ref (sv0 {svTrace = tr, svTracePos = 0, svStackFrame = 0})
    processSelectorUpdate ref
    
    sv1 <- readIORef ref
    -- initialise trace
    let tr = [TraceEntry { teStack = stackFromStore sv1 (fromJust $ D.sConcrete $ svState sv1) (svPID sv1)
                         , teStore = store}]
    writeIORef ref sv1{svTrace = tr, svTracePos = 0, svStackFrame = 0}
    updateDisplays ref

-- Disable all controls
disable :: RSourceView c a -> IO ()
disable ref = do
    -- disable process selector
    processSelectorDisable ref
    -- disable other displays
    commandButtonsDisable  ref
    sourceWindowDisable    ref
    stackViewDisable       ref
    traceViewDisable       ref
    watchDisable           ref
    resolveViewDisable     ref
    actionSelectorDisable  ref

-- Access current location in the trace 

currentCFA :: SourceView c a -> CFA
currentCFA sv = getCFA sv (svTracePos sv)

currentLoc :: SourceView c a -> Loc
currentLoc sv = getLoc sv (svTracePos sv)

currentLocLabel :: SourceView c a -> LocLabel
currentLocLabel sv = getLocLabel sv (svTracePos sv)

currentDelay :: SourceView c a -> Bool
currentDelay sv = getDelay sv (svTracePos sv)

currentStore :: SourceView c a -> Store
currentStore sv = getStore sv (svTracePos sv)

modifyStore :: SourceView c a -> Int -> (Store -> Store) -> SourceView c a
modifyStore sv idx f = sv {svTrace = tr'}
    where tr     = svTrace sv
          entry  = tr !! idx
          entry' = entry {teStore = (f $ teStore entry)}
          tr'    = take idx tr ++ [entry'] ++ drop (idx+1) tr

modifyCurrentStore :: SourceView c a -> (Store -> Store) -> SourceView c a
modifyCurrentStore sv f = modifyStore sv (svTracePos sv) f

currentStack :: SourceView c a -> ProcStack
currentStack sv = getStack sv (svTracePos sv)

currentTmpExprTree :: SourceView c a -> Forest Expr
currentTmpExprTree sv = getTmpExprTree sv (svTracePos sv)

currentControllable :: SourceView c a -> Bool
currentControllable sv = storeEvalBool (currentStore sv) (EVar mkContVarName)

currentMagic :: SourceView c a -> Bool
currentMagic sv = storeEvalBool (currentStore sv) (EVar mkMagicVarName)

currentError :: SourceView c a -> Bool
currentError sv = storeEvalBool (currentStore sv) mkErrVar

cfaAtFrame :: SourceView c a -> Int -> CFA
cfaAtFrame sv frame = trace ("cfaAtFrame " ++ showStack (drop frame $ currentStack sv)) $ stackGetCFA sv (svPID sv) (drop frame $ currentStack sv)

-- Access arbitrary location in the trace 

getCFA :: SourceView c a -> Int -> CFA
getCFA sv p = stackGetCFA sv (svPID sv) (getStack sv p)

stackGetCFA :: SourceView c a -> PID -> ProcStack -> CFA
-- HACK
stackGetCFA sv ["$init"] _                                                                                           = tranCFA $ fst $ tsInit $ specTran (svSpec sv)
stackGetCFA sv _   ((FrameCTask   (Front.ScopeMethod _ m) _):_)                                                      = specGetCFA (svSpec sv) []  (Just $ sname m)
stackGetCFA sv pid ((FrameRegular (Front.ScopeMethod _ m) _):_) | Front.methCat m == Front.Task Front.Controllable   = specGetCFA (svSpec sv) pid (Just $ sname m)
                                                                | Front.methCat m == Front.Task Front.Uncontrollable = specGetCFA (svSpec sv) pid (Just $ sname m) 
stackGetCFA sv pid ((FrameRegular (Front.ScopeProcess _ _) _):_)                                                     = specGetCFA (svSpec sv) pid Nothing
stackGetCFA _  _   ((FrameInteractive _ _ cfa):_)                                                                    = cfa
stackGetCFA sv pid (_:stack)                                                                                         = stackGetCFA sv pid stack
        

getLoc :: SourceView c a -> Int -> Loc
getLoc sv p = frLoc $ head $ getStack sv p

getLocLabel :: SourceView c a -> Int -> LocLabel
getLocLabel sv p = cfaLocLabel (getLoc sv p) (getCFA sv p)

getDelay :: SourceView c a -> Int -> Bool
getDelay sv p = isDelayLabel $ getLocLabel sv p

getStore :: SourceView c a -> Int -> Store
getStore sv p | p >= (length $ svTrace sv) = SStruct $ M.empty
              | otherwise                  = teStore $ svTrace sv !! p

getStack :: SourceView c a -> Int -> ProcStack
getStack sv p = teStack $ svTrace sv !! p

getTmpExprTree :: SourceView c a -> Int -> Forest Expr
getTmpExprTree sv p =
    let ?spec = svSpec sv in
    let -- collect tmp variables from all transitions from the current location
        trans = map snd $ Graph.lsuc (getCFA sv p) (getLoc sv p)
        -- expand expression into a tree of scalars
        mkTree e = Node { rootLabel = e
                        , subForest = map mkTree 
                                      $ case typ e of
                                             Struct fs  -> map (\(Field n _) -> EField e n) fs
                                             Array _ sz -> map (EIndex e . EConst . UIntVal 32 . fromIntegral) [0..sz-1]
                                             _          -> []
                          }
    in map (mkTree . EVar . varName)
       $ filter ((== VarTmp) . varCat)
       $ nub        
       $ concatMap (\t -> case t of
                               TranStat (SAssume e)   -> exprVars e
                               TranStat (SAssign _ e) -> exprVars e
                               _                      -> []) 
       $ trans


-- Execute one CFA transition
-- Returns True if the step was performed successfully and
-- False otherwise (i.e., the user did not provide values
-- for nondeterministic arguments)
microstep :: SourceView c a -> Maybe (SourceView c a)
microstep sv = 
    -- Try all transitions from the current location; choose the first successful one
    let transitions = Graph.lsuc (currentCFA sv) (currentLoc sv)
    in case mapMaybe (microstep' sv) transitions of
            []               -> trace "microstep' returns false" 
                                $ Nothing
            (store, stack):_ -> trace ("microstep': stack=" ++ showStack stack) 
                                $ Just $ traceAppend sv store stack

microstep' :: SourceView c a -> (Loc, TranLabel) -> Maybe (Store, ProcStack)
microstep' sv (to, TranCall meth mretloc)  = -- insert new stack frame and mofify the old frame to point to return location,
                                             -- so that Return can be performed later
                                             let ?spec = svFlatSpec sv in
                                             let f0:frames = currentStack sv
                                                 stack' = case mretloc of
                                                               Nothing -> f0 : frames
                                                               Just l  -> f0{frLoc = l} : frames
                                                 sc = Front.ScopeMethod tmMain meth
                                             in if frScope f0 == sc -- avoid creating duplicate frames (happens with task calls)
                                                   then Just (currentStore sv, f0{frLoc=to} : frames)
                                                   else Just (currentStore sv, (FrameRegular sc to) : stack')
microstep' sv (_ , TranReturn)             = Just (currentStore sv, tail $ currentStack sv)
microstep' sv (to, TranNop)                = Just (currentStore sv, (head $ currentStack sv){frLoc = to} : (tail $ currentStack sv))
microstep' sv (to, TranStat (SAssume e))   = if storeEvalBool (currentStore sv) e == True
                                                then Just (currentStore sv, (head $ currentStack sv){frLoc = to} : (tail $ currentStack sv))
                                                else Nothing
microstep' sv (to, TranStat (SAssign l r)) = trace ("SAssign: " ++ show l ++ ":=" ++ show r) $
                                             let rval = storeTryEval (currentStore sv) r
                                                 store' = storeSet (currentStore sv) l rval
                                             in case rval of 
                                                     Nothing -> Nothing
                                                     _       -> Just (store', (head $ currentStack sv){frLoc = to} : (tail $ currentStack sv))

---- Actions taken upon reaching a delay location
--maybeCompleteTransition :: SourceView c a -> SourceView c a
--maybeCompleteTransition sv | currentDelay sv && currentControllable sv = sv
--                           | currentDelay sv                           = setPC pid pc sv
--                           | otherwise                                 = sv
--    -- update PC and PID variables
--    where pc = currentLoc sv
--          mmeth = fmap sname $ stackTask (currentStack sv) 0
--          pid = svPID sv ++ (maybeToList mmeth)

makeTransition :: (D.Rel c v a s) => RSourceView c a -> IO ()
makeTransition ref = do
    putStrLn "makeTransition"
    sv <- readIORef ref
    model <- readIORef $ svModel sv
    let ?spec    = svSpec sv
        ?absvars = svAbsVars sv
        ?model   = model
        ?m       = D.mCtx model
    -- abstract final state
    let trans = D.abstractTransition (svState sv) (currentStore sv) 
    -- add transition
    D.modelAddTransition (svModel sv) trans
--    D.modelSelectState (svModel sv) (Just $ D.tranTo trans)


setPID :: PID -> SourceView c a -> SourceView c a
setPID pid sv = modifyCurrentStore sv (\s -> storeSet s mkPIDVar (Just $ SVal $ EnumVal $ mkPIDEnumeratorName pid))
    
setLPID :: PID -> SourceView c a -> SourceView c a
setLPID pid sv = modifyCurrentStore sv (\s -> storeSet s mkPIDLVar (Just $ SVal $ EnumVal $ mkPIDEnumeratorName pid))

maybeSetLCont :: SourceView c a -> SourceView c a
maybeSetLCont sv | (isNothing $ storeTryEvalBool (currentStore sv) mkContLVar) = 
                   modifyCurrentStore sv (\s -> storeSet s mkContLVar $ Just $ SVal $ BoolVal False)
                 | otherwise                                                  = sv

-- Evaluate expression written in terms of variables in the original input spec.
storeEvalStr :: Front.Spec -> Front.Spec -> Store -> PID -> Front.Scope -> String -> Either String Store
storeEvalStr inspec flatspec store pid sc str = do
    -- Apply all transformations that the input spec goes through to the expression:
    -- 1. parse
    expr <- case parse (Parse.detexpr <* eof) "" str of
                 Left  e  -> Left $ show e
                 Right ex -> Right ex
    let (scope, iid) = flatScopeToScope inspec sc
    -- 2. validate
    let ?spec  = inspec
        ?privoverride = True 
    Front.validateExpr scope expr
    let ?scope = scope
        in when (not $ Front.exprNoSideEffects expr) $ throwError "Expression has side effects"
    -- 3. flatten
    let flatexpr = Front.exprFlatten iid scope expr
    -- 4. simplify
    let ?spec = flatspec
    let (ss, simpexpr) = let ?scope = sc
                         in evalState (Front.exprSimplify flatexpr) (0,[])
    when (not $ null ss) $ throwError "Expression too complex"
    -- 5. inline
    let lmap = scopeLMap pid sc
    let ctx = CFACtx { ctxPID     = pid
                     , ctxCont    = error "evalStr: ctxCont undefined"
                     , ctxStack   = [(sc, error "evalStr: return", Nothing, lmap)]
                     , ctxCFA     = error "evalStr: CFA undefined"
                     , ctxBrkLocs = []
                     , ctxGNMap   = globalNMap
                     , ctxLastVar = 0
                     , ctxVar     = []}
        iexpr = evalState (Front.exprToIExprDet simpexpr) ctx
    -- 6. evaluate
    return $ storeEval store iexpr

compileControllableAction :: Front.Spec -> Front.Spec -> PID -> Front.Scope -> String -> FilePath -> Either String CFA
compileControllableAction inspec flatspec pid sc str fname = do
    trace ("compileControllableAction" ++ show pid) $ return ()
    -- Apply all transformations that the input spec goes through to the statement:
    -- 1. parse
    stat <- liftM (Front.sSeq nopos)
            $ case parse (Parse.statements1 <* eof) fname str of
                   Left  e  -> Left $ show e
                   Right st -> Right st
    let (scope,iid) = flatScopeToScope inspec sc
    -- 2. validate
    let ?spec = inspec
        ?privoverride = False
    Front.validateStat scope stat
    validateControllableStat scope stat
    -- 3. flatten
    let flatstat = Front.statFlatten iid scope stat
    -- 4. simplify
    let ?spec = flatspec
    let (simpstat, (_, vars)) = let ?scope = sc
                                in runState (Front.statSimplify flatstat) (0,[])
    assert (null vars) (pos stat) "Statement too complex"
    -- 5. inline
    let ctx = CFACtx { ctxPID     = pid
                     , ctxCont    = True
                     , ctxStack   = []
                     , ctxCFA     = newCFA sc simpstat true
                     , ctxBrkLocs = []
                     , ctxGNMap   = globalNMap
                     , ctxLastVar = 0
                     , ctxVar     = []}
        ctx' = let ?procs =[] in execState (do -- create final state and make it the return location
                                               aftret <- ctxInsLocLab (LFinal ActNone [])
                                               ctxPushScope sc aftret Nothing (scopeLMap pid sc)
                                               aftstat <- Front.procStatToCFA simpstat cfaInitLoc
                                               -- switch to uncontrollable state
                                               aftucont <- ctxInsTrans' aftstat $ TranStat $ mkContVar =: false
                                               aftpid <- ctxInsTrans' aftucont $ TranStat $ mkPIDVar =: (EConst $ EnumVal $ mkPIDEnumeratorName pidCont)
                                               -- add return after the statement to pop FrameInteractive off the stack
                                               ctxInsTrans aftpid aftret TranReturn
                                               ) ctx
    assert (null $ ctxVar ctx') (pos stat) "Cannot perform non-deterministic controllable action"
    -- Prune the resulting CFA beyond the first pause location; add a return transition in the end
    let cfa   = ctxCFA ctx'
        cfar  = cfaPruneUnreachable cfa [cfaInitLoc]
        reach = cfaReachInst cfar cfaInitLoc
        cfa'  = cfaPrune cfar (S.insert cfaInitLoc reach)
    assert (Graph.noNodes cfar == Graph.noNodes cfa') (pos stat) "Controllable action must be an instantaneous statement"
    return $ cfaTraceFile cfa' "action" cfa'
    
-- Check whether statement specifies a valid controllable action:
-- * Function, procedure, and controllable task calls only
-- * No fork statements
-- * No variable declarations
-- * No pause or stop statements
-- * No assert or assume
-- * No magic blocks
validateControllableStat :: (?spec::Front.Spec) => Front.Scope -> Front.Statement -> Either String ()
validateControllableStat sc stat = do
    _ <- mapM (\(p,(_,m)) -> do assert (Front.methCat m /= Front.Task Front.Uncontrollable) p "Uncontrollable task invocations are not allowed inside controllable actions"
                                assert (Front.methCat m /= Front.Task Front.Invisible)      p "Invisible task invocations are not allowed inside controllable actions")
         $ Front.statCallees sc stat
    _ <- Front.mapStatM (\_ st -> case st of
                                       Front.SVarDecl p _   -> err p "Variable declarations are not allowed inside controllable actions"
                                       Front.SReturn  p _   -> err p "Return statements are not allowed inside controllable actions"
                                       Front.SPar     p _   -> err p "Fork statements are not allowed inside controllable actions"
                                       Front.SPause   p     -> err p "Pause statements are not allowed inside controllable actions"
                                       Front.SWait    p _   -> err p "Wait statements are not allowed inside controllable actions"
                                       Front.SStop    p     -> err p "Stop statements are not allowed inside controllable actions"
                                       Front.SAssert  p _   -> err p "Assertions are not allowed inside controllable actions"
                                       Front.SAssume  p _   -> err p "Assume statements are not allowed inside controllable actions"
                                       Front.SMagic   p _ _ -> err p "Magic blocks are not allowed inside controllable actions"
                                       _                    -> return st) sc stat
    return ()

flatScopeToScope :: Front.Spec -> Front.Scope -> (Front.Scope, Front.IID)
flatScopeToScope inspec sc = 
    let ?spec = inspec in
    case sc of
         Front.ScopeMethod   _ meth -> let (i, mname) = Front.itreeParseName $ name meth
                                           tm = Front.itreeTemplate i
                                           meth' = fromJust $ find ((== mname) . Front.methName) $ Front.tmAllMethod tm
                                       in (Front.ScopeMethod tm meth', i)
         Front.ScopeProcess  _ proc -> let (i, pname) = Front.itreeParseName $ name proc
                                           tm = Front.itreeTemplate i
                                           proc' = fromJust $ find ((== pname) . Front.procName) $ Front.tmAllProcess tm
                                       in (Front.ScopeProcess tm proc', i)
         Front.ScopeTemplate _      -> (Front.ScopeTop, [])
