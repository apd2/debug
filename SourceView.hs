{-# LANGUAGE ImplicitParams, RecordWildCards, ScopedTypeVariables, TupleSections #-}

module SourceView(sourceViewNew, 
                  simulateTransition,
                  contTransToSource) where

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

import PP
import PID
import qualified Grammar
import Util hiding (name, trace)
import TSLUtil
import qualified DbgTypes      as D
import qualified IDE           as D
import qualified DbgAbstract   as D
--import qualified DbgConcretise as D
import ISpec
import IExpr
import IVar
import IType
import CFA
import Inline
import Predicate
import Store
import SMTSolver

import qualified NS              as F
import qualified Method          as F
import qualified Process         as F
import qualified InstTree        as F
import qualified Template        as F
import qualified TemplateOps     as F
import qualified ExprInline      as F
import qualified Spec            as F
import qualified Expr            as F
import qualified ExprFlatten     as F
import qualified ExprOps         as F
import qualified ExprValidate    as F
import qualified Statement       as F
import qualified StatementOps    as F
import qualified StatementInline as F
import qualified Name            as F
import qualified Type            as F
import qualified Pos             as F
import qualified TypeOps         as F

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

data ProcStackFrame = FrameRegular      {frScope::F.Scope, frLoc::Loc}
                    | FrameControllable {frScope::F.Scope, frLoc::Loc, frCFA::CFA}

isFrameControllable (FrameControllable _ _ _) = True
isFrameControllable _                         = False

instance PP ProcStackFrame where
    pp f = (if' (isFrameControllable f) (PP.text "(interactive)") PP.empty) PP.<+> (PP.text $ show $ frScope f) PP.<> PP.char ':' PP.<+> (PP.text $ show $ frLoc f)

type ProcStack = [ProcStackFrame]

instance PP ProcStack where
    pp stack = PP.vcat $ map pp stack

showStack :: ProcStack -> String
showStack = PP.render . pp

stackToProcStack :: Stack -> ProcStack
stackToProcStack frames = map (\(Frame sc loc) -> FrameRegular sc loc) frames

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
    svInputSpec      :: F.Spec,
    svFlatSpec       :: F.Spec,
    svAbsVars        :: M.Map String AbsVar,
    svState          :: D.State a Store,            -- current state set via view callback
    svTmp            :: Store,                      -- temporary variables store
    svSolver         :: SMTSolver,

    -- Command buttons
    svStepButton     :: G.ToolButton,
    svRunButton      :: G.ToolButton,
    svMagExitButton  :: G.ToolButton,               -- exit magic block
    svContButton     :: G.ToolButton,               -- switch to controllable state

    -- Trace
    svTrace          :: Trace,                      -- steps from the current state
    svTracePos       :: Int,                        -- current position in the trace
    svTraceCombo     :: G.ComboBox,                 -- trace combo box
    svTraceStore     :: G.ListStore Int,            -- indices in the trace that correspond to visible locations
    svTraceUndo      :: G.ToolButton,               -- back button
    svTraceRedo      :: G.ToolButton,               -- forward button

    -- Process selector
    svPID            :: PrID,                       -- PID set in the process selection menu
    svProcessCombo   :: G.ComboBox,                 -- process selection combo box
    svProcessStore   :: G.TreeStore (PrID, Bool),   -- tree store that backs the process selector

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
    svInprogLab      :: G.Label,                    --                transition in progress

    -- Resolve view
    svResolveStore   :: G.TreeStore Expr,           -- tmp variables in the scope of the current expression
    svAutoResolve    :: Bool,                       -- resolve non-determinism automatically
    svAutoResolveTog :: G.CheckButton,              -- toggle auto-resolve mode button

    -- Action selector
    svActSelectText  :: G.TextView,                 -- user-edited controllable action
    svActSelectBAdd  :: G.Button,                   -- perform controllable action button
    svFromSrc        :: Bool                        -- executing transition from action selector
}

type RSourceView c a = IORef (SourceView c a)

sourceViewEmpty = SourceView { svModel          = error "SourceView: svModel undefined"
                             , svInputSpec      = error "SourceView: svInputSpec undefined"
                             , svFlatSpec       = error "SourceView: svFlatSpec undefined"
                             , svSpec           = error "SourceView: svSpec undefined" 
                             , svAbsVars        = error "SourceView: svAbsVars undefined"
                             , svState          = error "SourceView: svState undefined"
                             , svTmp            = error "SourceView: svTmp undefined"
                             , svSolver         = error "SourceView: svSolver undefined"
                             , svStepButton     = error "SourceView: svStepButton undefined"
                             , svRunButton      = error "SourceView: svRunButton undefined"
                             , svMagExitButton  = error "SourceView: svMagExitButton undefined"
                             , svContButton     = error "SourceView: svContButton undefined"
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
                             , svFromSrc        = False
                             }


--------------------------------------------------------------
-- View callbacks
--------------------------------------------------------------

sourceViewNew :: (D.Rel c v a s) => F.Spec -> F.Spec -> Spec -> M.Map String AbsVar -> SMTSolver -> D.RModel c a Store -> IO (D.View a Store)
sourceViewNew inspec flatspec spec absvars solver rmodel = do

    ref <- newIORef $ sourceViewEmpty { svModel          = rmodel
                                      , svInputSpec      = inspec
                                      , svFlatSpec       = flatspec
                                      , svSpec           = specInlineWireAlways spec
                                      , svAbsVars        = absvars
                                      , svSolver         = solver
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

    bcont <- G.toolButtonNewFromStock G.stockIndex
    G.set bcont [G.widgetTooltipText G.:= Just "Switch to Controllable State"]
    _ <- G.onToolButtonClicked bcont (switchToControllable ref)
    G.widgetShow bcont
    G.toolbarInsert tbar bcont (-1)    

    modifyIORef ref (\sv -> sv { svRunButton     = butrun
                               , svStepButton    = butstep
                               , svMagExitButton = bexit
                               , svContButton    = bcont})

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

--    -- HACK: create an initial state
--    model <- readIORef rmodel
--    when (isNothing $ find ((== "init") . fst) $ D.mStateRels model) $ do
--        let ?spec    = spec
--            ?absvars = absvars
--            ?model   = model
--            ?m       = D.mCtx model
--        initstore <- liftM storeExtendDefaultState
--                     $ case smtGetModel solver [let ?pred = [] in bexprToFormula $ snd $ tsInit $ specTran spec] of
--                            Just (Right store) -> return store                      
--                            _                  -> fail "Unsatisfiable initial condition"
--        -- more hack: simulate init transition
--        modifyIORef ref (\sv -> sv { svState      = D.abstractState initstore
--                                   , svTmp        = SStruct $ M.empty
--                                   , svPID        = PrID "$init" []
--                                   , svTrace      = [TraceEntry { teStack = [FrameRegular (F.ScopeTemplate (let ?spec = flatspec in tmMain)) (tranFrom $ fst $ tsInit $ specTran ?spec)]
--                                                                , teStore = initstore}]
--                                   , svTracePos   = 0
--                                   , svStackFrame = 0})
--
--        runAction ref
--        initstore' <- getIORef currentStore ref
--        D.modelSelectState rmodel (Just $ D.abstractState initstore') 
--    -- END HACK

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
    modifyIORef ref (\sv -> sv { svState    = s
                               , svTmp      = SStruct $ M.empty})
    processSelectorChooseUniqueEnabled ref
    reset ref
    putStrLn "sourceViewStateSelected done"

sourceViewTransitionSelected :: (D.Rel c v a s) => RSourceView c a -> D.Transition a Store -> IO ()
sourceViewTransitionSelected ref tran | (not $ D.isConcreteTransition tran) = disable ref
                                      | otherwise                           = do
    putStrLn "sourceViewTransitionSelected"
    modifyIORef ref (\sv -> sv { svState    = D.tranFrom tran
                               , svTmp      = fromJust $ D.tranConcreteLabel tran})
    let cont = storeEvalBool (fromJust $ D.sConcrete $ D.tranFrom tran) mkContVar
    when (cont) $ maybe (return ()) (\str -> do txt <- getIORef svActSelectText ref
                                                buf <- G.textViewGetBuffer txt
                                                G.set buf [G.textBufferText G.:= str])
                        (D.tranSrc tran)
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
    let sv0 = maybeSetLCont $ setLEPID (stackGetEPID (svPID sv) $ currentStack sv) sv
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
    let sv0 = maybeSetLCont $ setLEPID (stackGetEPID (svPID sv) $ currentStack sv) sv
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
    modifyIORef ref (\sv -> setEPID EPIDCont 
                            $ modifyCurrentStore sv (\st0 -> let st1 = storeSet st0 mkMagicVar (Just $ SVal $ BoolVal False)
                                                                 st2 = storeSet st1 mkContVar  (Just $ SVal $ BoolVal False)
                                                             in st2))
    makeTransition ref

-- simulate transition without GUI 
simulateTransition :: F.Spec -> Spec -> M.Map String AbsVar -> Store -> Store -> Maybe Store
simulateTransition flatspec spec absvars st lab =
    let -- create enough of source view to call run
        sv0 :: SourceView () ()
        sv0 = sourceViewEmpty { svSpec       = spec
                              , svFlatSpec   = flatspec
                              , svAbsVars    = absvars
                              , svState      = D.State { sAbstract = error "simulateTransition: sAbstract is undefined"
                                                       , sConcrete = Just st}
                              , svTmp        = lab
                              , svTracePos   = 0
                              , svStackFrame = 0
                              }
        pid = case parseEPIDEnumerator flatspec $ storeEvalEnum lab mkEPIDLVar of
                   EPIDProc p -> p
                   _          -> -- find process currently inside a magic block
                                 case findProcInsideMagic sv0 of 
                                      Just p -> p
                                      _      -> error $ "simulateTransition no process inside a magic block"

        sv1 = if storeEvalBool st (EVar mkContVarName)
                 then -- execute controllable CFA
                      sv0 { svPID   = pid
                          , svTrace = [TraceEntry { teStore = storeUnion st lab
                                                  , teStack = [FrameControllable F.ScopeTop cfaInitLoc (specCAct spec)]}]}
                 else -- execute uncontrollable process from its current location
                      sv0 { svPID   = pid
                          , svTrace = [TraceEntry { teStore = storeUnion st lab 
                                                  , teStack = stackFromStore sv0 st pid}]} 
        msv2 = run sv1 
        mstore2 = case msv2 of
                       Nothing  -> trace ("simulateTransition: msv2 = Nothing") Nothing
                       Just sv2 -> if not $ currentDelay sv2
                                      then Nothing
                                      else Just $ currentStore sv2
    in trace ("simulateTransition\nlabel: " ++ show lab ++ "\nstate: " ++ show st)
       $ mstore2

contTransToSource :: F.Spec -> F.Spec -> Spec -> D.Transition a Store -> Maybe String
contTransToSource inspec flatspec spec D.Transition{..} = do
    iid <- findActiveMagicBlock flatspec spec (fromJust $ D.sConcrete tranFrom)
    act <- transitionToAction inspec flatspec spec (storeUnion (fromJust $ D.sConcrete tranTo) (fromJust tranConcreteLabel))
    doc <- ppContAction inspec iid act
    return $ PP.render doc

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
                     G.set rend [G.cellTextMarkup G.:= Just $ "<span weight=\"" ++ (if en then "bold" else "normal") ++ "\">" ++ show pid ++ "</span>"])
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
           modifyIORef ref (\_sv -> _sv{svPID = pid})
           --cfaShow (specGetCFA (svSpec sv) pid Nothing) (pidToName pid)
           reset ref


pidtree :: SourceView c a -> Forest PrID 
pidtree sv = map (\p -> procTree (PrID (procName p) []) p) (specProc $ svSpec sv)
    where procTree pid p = Node { rootLabel = pid
                                , subForest = map (\p' -> procTree (childPID pid (procName p')) p') (procChildren p)}

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

processSelectorSelectPID :: RSourceView c a -> PrID -> IO ()
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
                     let txt = take 48  
                               $ replace "\r" " " $ replace "\n" " " 
                               $ case locAct $ getLocLabel sv idx of
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
    --putStrLn $ "traceViewUpdate:\n" ++ (showTrace $ svTrace sv)
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
                                                                                             (currentStore sv) 
                                                                                             (Just svPID) 
                                                                                             (frScope $ currentStack sv !! svStackFrame) e of
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
    G.widgetSetSizeRequest view 600 600
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

sourceWindowUpdate :: RSourceView c a -> IO ()
sourceWindowUpdate ref = do
    sv <- readIORef ref
    let cfa = cfaAtFrame sv (svStackFrame sv)
        loc = frLoc $ (currentStack sv) !! (svStackFrame sv)
    case locAct $ cfaLocLabel loc cfa of
         ActNone   -> do sourceClearPos sv
                         G.labelSetText (svFileLab sv) ""
         ActExpr e -> do sourceSetPos sv (F.pos e) colorUCont
                         G.labelSetText (svFileLab sv) (sourceName $ fst $ F.pos e)
         ActStat (F.SMagic p mp _) -> do let col = if' (currentControllable sv) colorCont colorUCont
                                         if currentMagic sv 
                                            then sourceSetPos sv mp col
                                            else sourceSetPos sv p  col
                                         G.labelSetText (svFileLab sv) (sourceName $ fst p)
         ActStat s -> do sourceSetPos sv (F.pos s) colorUCont
                         G.labelSetText (svFileLab sv) (sourceName $ fst $ F.pos s)
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

sourceSetPos :: SourceView c a -> F.Pos -> String -> IO ()
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
    let pen = isProcEnabled sv (svPID sv)
    let lab = currentLocLabel sv
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
                                              && (svTracePos sv == 0)                    -- there is no transition in progress
                                              && isInsideMagicBlock (currentLocLabel sv) -- current process is inside the MB
    G.widgetSetSensitive (svContButton sv) $  (currentMagic sv)                       -- we're inside a magic block
                                           && (svTracePos sv == 0)                    -- there is no transition in progress
                                           && (not $ currentControllable sv)          -- we're in an uncontrollable state
                                           && isInsideMagicBlock (currentLocLabel sv) -- current process is inside the MB

commandButtonsDisable :: RSourceView c a -> IO ()
commandButtonsDisable ref = do
    sv <- readIORef ref
    -- disable command buttons
    G.widgetSetSensitive (svStepButton sv)    False
    G.widgetSetSensitive (svRunButton sv)     False
    G.widgetSetSensitive (svMagExitButton sv) False
    G.widgetSetSensitive (svContButton sv)    False

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
    case compileControllableAction svSolver svInputSpec svFlatSpec svPID (frScope $ head $ currentStack sv) text fpath of
         Left e    -> D.showMessage svModel G.MessageError e
         Right cfa -> do switchToControllable ref 
                         writeFile fpath text
                         runControllableCFA ref cfa
    
switchToControllable :: (D.Rel c v a s) => RSourceView c a -> IO ()
switchToControllable ref = do
    sv0 <- readIORef ref
    let Just pid = findProcInsideMagic sv0
    let sv1 = modifyCurrentStore sv0 (\st0 -> storeSet st0 mkContVar (Just $ SVal $ BoolVal True))
        sv2 = setEPID (EPIDProc pid) sv1
    when (not $ storeEvalBool (currentStore sv0) mkContVar) $ do
        writeIORef ref sv2
        makeTransition ref

runControllableCFA :: RSourceView c a -> CFA -> IO ()
runControllableCFA ref cfa = do
    -- Push controllable cfa on the stack
    modifyIORef ref (\sv -> let stack = currentStack sv
                                stack' = (FrameControllable (frScope $ head stack) cfaInitLoc cfa) : stack
                            in (traceAppend sv (currentStore sv) stack'){svFromSrc = True})
    updateDisplays ref

actionSelectorUpdate :: RSourceView c a -> IO ()
actionSelectorUpdate ref = do
    sv <- readIORef ref
--    putStrLn $ "actionSelectorUpdate: controllable=" ++ 
--               (show $ currentControllable sv) ++ 
--               " waitformagic=" ++ 
--               (show $ isWaitForMagicLabel $ currentLocLabel sv)
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
-- Generating source code for controllable transitions
--------------------------------------------------------------

-- TODO: add idle action
data ContAction = ActExit  -- exit magic block
                | ActCall {caIID :: F.IID, caTask :: F.Method, caArgs :: [(String, F.Expr)]}

ppContAction :: F.Spec ->  F.IID -> ContAction -> Maybe PP.Doc
ppContAction _      _   ActExit                = Just $ PP.text "exit"
ppContAction inspec iid (ActCall methiid t as) = do 
    path <- let ?spec = inspec in F.itreeAbsToRelPath iid methiid
    return $ (PP.hcat $ PP.punctuate (PP.char '.') $ (map (PP.text . F.sname) path) ++ [PP.text $ F.sname t]) PP.<>
             (PP.parens $ PP.hcat $ PP.punctuate PP.comma $ map (\a -> if' (F.argDir a == F.ArgOut) (PP.char '_') (pp $ fromJust $ lookup (F.sname a) as)) $ F.methArg t) PP.<>
             PP.semi
    
-- Translate an abstract transition into a controllable action
-- cstate - concrete state before the transition
-- alabel - abstract transition label
transitionToAction :: F.Spec -> F.Spec -> Spec -> Store -> Maybe ContAction
transitionToAction inspec flatspec spec cnstate = do
    let ?spec = inspec
    let tag = storeEvalEnum cnstate mkTagVar
    if tag == mkTagIdle
       then if not $ storeEvalBool cnstate mkMagicVar
               then return ActExit
               else Nothing
       else do let flatmeth = let ?spec = flatspec in fromJust $ find ((== tag) . F.sname) $ F.tmMethod tmMain
               let (caIID, methname) = F.itreeParseName (F.Ident F.nopos tag)
                   Just (F.ObjMethod tm caTask) = F.lookupGlobal ((F.Ident F.nopos "main"):caIID++[methname])
               let caArgs = map (\a -> (F.sname a, let ?scope = F.ScopeTemplate tm in storeToFExpr a
                                                   $ storeEval cnstate 
                                                   $ (EVar $ mkVarNameS (NSID Nothing (Just flatmeth)) $ F.sname a)))
                            $ filter ((== F.ArgIn) . F.argDir)
                            $ F.methArg caTask
               return ActCall{..}


storeToFExpr :: (?spec::F.Spec, F.WithType a) => a -> Store -> F.Expr
storeToFExpr x s = 
    let F.Type xsc  ts  = F.typ x
        F.Type xsc' ts' = F.typ' x
    in case ts' of
            F.StructSpec _ fs  -> F.EStruct F.nopos tname' (Left $ map (\f -> (F.name f, let ?scope = xsc' in storeToFExpr f (sfs M.! F.sname f))) fs)
                                  where F.UserTypeSpec _ tname = ts
                                        (decl, tsc) = F.getTypeDecl xsc tname
                                        tname' = case tsc of 
                                                      F.ScopeTop         -> [F.name decl]
                                                      F.ScopeTemplate tm -> [F.name tm, F.name decl]
                                        SStruct sfs = s
            F.ArraySpec  _ _ _ -> error "storeToFExpr does not support array arguments"
            _                  -> valToFExpr (F.tspec $ F.typ' x) v
                                  where SVal v = s

valToFExpr :: F.TypeSpec -> Val -> F.Expr
valToFExpr (F.BoolSpec _)   (BoolVal True)  = F.EBool F.nopos True
valToFExpr (F.BoolSpec _)   (BoolVal False) = F.EBool F.nopos False
valToFExpr (F.SIntSpec _ w) (SIntVal _ i)   = F.ELit  F.nopos w True  F.Rad10 i
valToFExpr (F.UIntSpec _ w) (UIntVal _ i)   = F.ELit  F.nopos w False F.Rad10 i
valToFExpr (F.EnumSpec _ _) (EnumVal e)     = F.ETerm F.nopos (F.unflattenName $ F.Ident F.nopos e)
valToFExpr ts               _               = error $ "valToFExpr: type " ++ show ts ++ " not supported"

-- Given a controllable state, find template instance that contains 
-- currently active magic block
findActiveMagicBlock :: F.Spec -> Spec -> Store -> Maybe F.IID
findActiveMagicBlock flatspec spec st = 
   let sv0 :: SourceView () ()
       sv0 = sourceViewEmpty { svSpec       = spec
                             , svFlatSpec   = flatspec
                             , svState      = D.State { sAbstract = error "findMagicBlock: sAbstract is undefined"
                                                      , sConcrete = Just st}
                             , svTracePos   = 0
                             , svStackFrame = 0
                             }
   in (fmap (\pid -> let stack = stackFromStore sv0 st pid
                         sc = frScope $ head stack
                         tm = case sc of
                                   F.ScopeProcess tm' _ -> tm'
                                   F.ScopeMethod  tm' _ -> tm'
                      in fst $ F.itreeParseName (F.name tm) )) 
      $ findProcInsideMagic sv0


--------------------------------------------------------------
-- Private helpers
--------------------------------------------------------------

isWaitForMagicLabel :: LocLabel -> Bool
isWaitForMagicLabel (LPause _ _ e) = isWaitForMagic e
isWaitForMagicLabel _              = False

-- Given a snapshot of the store at a pause location, compute
-- process stack.
stackFromStore :: SourceView c a -> Store -> PrID -> ProcStack
stackFromStore sv s pid = stackFromStore' sv s pid

stackFromStore' :: SourceView c a -> Store -> PrID -> ProcStack
stackFromStore' sv s pid = stackToProcStack (locStack lab)
    where cfa   = specGetCFA (svSpec sv) (EPIDProc pid)
          loc   = storeGetLoc s pid
          lab   = cfaLocLabel loc cfa

-- Find out what the process is about to do (or is currently doing) based on its stack:
-- run a normal transition, a controllable task, or a controllable transition
stackGetEPID :: PrID -> ProcStack -> EPID
stackGetEPID pid stack = stackGetEPID' (EPIDProc pid) (reverse stack) 

stackGetEPID' epid []                                         = epid
stackGetEPID' _    ((FrameControllable _ _ _):_)              = EPIDCont
stackGetEPID' epid (_:s)                                      = stackGetEPID' epid s

stackGetCFA :: SourceView c a -> PrID -> ProcStack -> CFA
stackGetCFA sv pid stack = stackGetCFA' sv (reverse stack) (EPIDProc pid)

stackGetCFA' :: SourceView c a -> [ProcStackFrame] -> EPID -> CFA
stackGetCFA' sv []                              epid                                                              = specGetCFA (svSpec sv) epid
stackGetCFA' sv ((FrameRegular _ _):s)          epid                                                              = stackGetCFA' sv s epid
stackGetCFA' _  ((FrameControllable _ _ cfa):_) _                                                                 = cfa

storeGetLoc :: Store -> PrID -> Loc
storeGetLoc s pid = pcEnumToLoc pc
    where pcvar = mkPCVar pid
          pc    = storeEvalEnum s pcvar

isProcEnabled :: SourceView c a -> PrID -> Bool
isProcEnabled sv pid = 
    let store = fromJust $ D.sConcrete $ svState sv
        stack = stackFromStore sv store pid
        frame = head stack
        loc   = frLoc frame
        cfa   = stackGetCFA sv pid stack
        lab   = cfaLocLabel loc cfa
        cont  = storeEvalBool store (EVar mkContVarName)
    in case storeTryEvalEnum (svTmp sv) mkEPIDLVar of
            Just e -> case parseEPIDEnumerator (svFlatSpec sv) e of
                           EPIDCont -> isInsideMagicBlock lab
                           epid     -> stackGetEPID pid stack == epid
            _      -> -- The process is always enabled if it is inside a magic block
                      -- Otherwise, the process is enabled if we are in an uncontrollable 
                      -- state and its pause condition holds
                      if' (isInsideMagicBlock lab) True $
                      if' cont False $
                      case lab of
                           LPause _ _ cond -> storeEvalBool store cond == True
                           LFinal _ _      -> not $ null $ Graph.lsuc cfa loc
                           _               -> True


isProcInsideMagic :: SourceView c a -> PrID -> Bool
isProcInsideMagic sv pid = isInsideMagicBlock lab || isFrameControllable frame 
    where
    store = fromJust $ D.sConcrete $ svState sv
    stack = stackFromStore sv store pid
    frame = head stack
    loc   = frLoc frame
    cfa   = stackGetCFA sv pid stack
    lab   = cfaLocLabel loc cfa

findProcInsideMagic :: SourceView c a -> Maybe PrID
findProcInsideMagic sv = find (isProcInsideMagic sv)
                         $ concatMap flatten $ pidtree sv

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
    writeIORef ref sv1{svTrace = tr, svTracePos = 0, svStackFrame = 0, svFromSrc = False}
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
cfaAtFrame sv frame = stackGetCFA sv (svPID sv) (drop frame $ currentStack sv)

-- Access arbitrary location in the trace 

getCFA :: SourceView c a -> Int -> CFA
getCFA sv p = stackGetCFA sv (svPID sv) $ getStack sv p

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
                                                 sc = F.ScopeMethod tmMain meth
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

makeTransition :: (D.Rel c v a s) => RSourceView c a -> IO ()
makeTransition ref = do
    putStrLn "makeTransition"
    sv@SourceView{..} <- readIORef ref
    model <- readIORef svModel
    let ?spec    = svSpec
        ?absvars = svAbsVars
        ?model   = model
        ?m       = D.mCtx model
    -- abstract final state
    let trans = D.abstractTransition svState (currentStore sv) 
    trans' <- if' svFromSrc (do buf  <- G.textViewGetBuffer svActSelectText
                                text <- G.get buf G.textBufferText
                                return $ trans{D.tranSrc = Just text})
                            (return $ trans{D.tranSrc = Just $ show svPID})
    -- add transition
    D.modelAddTransition svModel trans'
--    D.modelSelectState (svModel sv) (Just $ D.tranTo trans)


setEPID :: EPID -> SourceView c a -> SourceView c a
setEPID epid sv = modifyCurrentStore sv (\s -> storeSet s mkEPIDVar (Just $ SVal $ EnumVal $ mkEPIDEnumeratorName epid))
    
setLEPID :: EPID -> SourceView c a -> SourceView c a
setLEPID epid sv = modifyCurrentStore sv (\s -> storeSet s mkEPIDLVar (Just $ SVal $ EnumVal $ mkEPIDEnumeratorName epid))

maybeSetLCont :: SourceView c a -> SourceView c a
maybeSetLCont sv | (isNothing $ storeTryEvalBool (currentStore sv) mkContLVar) = 
                   modifyCurrentStore sv (\s -> storeSet s mkContLVar $ Just $ SVal $ BoolVal False)
                 | otherwise                                                  = sv

-- Evaluate expression written in terms of variables in the original input spec.
storeEvalStr :: F.Spec -> F.Spec -> Store -> Maybe PrID -> F.Scope -> String -> Either String Store
storeEvalStr inspec flatspec store mpid sc str = do
    -- Apply all transformations that the input spec goes through to the expression:
    -- 1. parse
    expr <- case parse (Grammar.detexpr <* eof) "" str of
                 Left  e  -> Left $ show e
                 Right ex -> Right ex
    let (scope, iid) = flatScopeToScope inspec sc
    -- 2. validate
    let ?spec  = inspec
        ?privoverride = True 
    F.validateExpr scope expr
    let ?scope = scope
        in when (not $ F.exprNoSideEffects expr) $ throwError "Expression has side effects"
    -- 3. flatten
    let flatexpr = F.exprFlatten iid scope expr
    -- 4. simplify
    let ?spec = flatspec
    let (ss, simpexpr) = let ?scope = sc
                         in evalState (F.exprSimplify flatexpr) (0,[])
    when (not $ null ss) $ throwError "Expression too complex"
    -- 5. inline
    let lmap = scopeLMap mpid sc
    let ctx = CFACtx { ctxEPID    = Nothing
                     , ctxStack   = [(sc, error "evalStr: return", Nothing, lmap)]
                     , ctxCFA     = error "evalStr: CFA undefined"
                     , ctxBrkLocs = []
                     , ctxGNMap   = globalNMap
                     , ctxLastVar = 0
                     , ctxVar     = []}
        iexpr = evalState (F.exprToIExprDet simpexpr) ctx
    -- 6. evaluate
    return $ storeEval store iexpr

compileControllableAction :: SMTSolver -> F.Spec -> F.Spec -> PrID -> F.Scope -> String -> FilePath -> Either String CFA
compileControllableAction solver inspec flatspec pid sc str fname = do
    trace ("compileControllableAction" ++ show pid) $ return ()
    -- Apply all transformations that the input spec goes through to the statement:
    -- 1. parse
    stat <- liftM (F.sSeq F.nopos)
            $ case parse (Grammar.statements1 <* eof) fname str of
                   Left  e  -> Left $ show e
                   Right st -> Right st
    let (scope,iid) = flatScopeToScope inspec sc
    -- 2. validate
    let ?spec = inspec
        ?privoverride = False
    F.validateStat scope stat
    validateControllableStat scope stat
    -- 3. flatten
    let flatstat = F.statFlatten iid scope stat
    -- 4. simplify
    let ?spec = flatspec
    let (simpstat, (_, vars)) = let ?scope = sc
                                in runState (F.statSimplify flatstat) (0,[])
    assert (null vars) (F.pos stat) "Statement too complex"
    -- 5. inline
    let ctx = CFACtx { ctxEPID    = Just EPIDCont
                     , ctxStack   = []
                     , ctxCFA     = newCFA sc simpstat true
                     , ctxBrkLocs = []
                     , ctxGNMap   = globalNMap
                     , ctxLastVar = 0
                     , ctxVar     = []}
        ctx' = let ?procs =[] in execState (do -- create final state and make it the return location
                                               aftret <- ctxInsLocLab (LFinal ActNone [])
                                               ctxPushScope sc aftret Nothing (scopeLMap (Just pid) sc)
                                               aftstat <- let ?solver = solver in F.procStatToCFA simpstat cfaInitLoc
                                               -- switch to uncontrollable state
                                               aftucont <- ctxInsTrans' aftstat $ TranStat $ mkContVar =: false
                                               aftpid <- ctxInsTrans' aftucont $ TranStat $ mkEPIDVar =: (EConst $ EnumVal $ mkEPIDEnumeratorName EPIDCont)
                                               -- add return after the statement to pop FrameInteractive off the stack
                                               ctxInsTrans aftpid aftret TranReturn
                                               ) ctx
    assert (null $ ctxVar ctx') (F.pos stat) "Cannot perform non-deterministic controllable action"
    -- Prune the resulting CFA beyond the first pause location; add a return transition in the end
    let cfa   = ctxCFA ctx'
        cfar  = cfaPruneUnreachable cfa [cfaInitLoc]
        reach = cfaReachInst cfar cfaInitLoc
        cfa'  = cfaPrune cfar (S.insert cfaInitLoc reach)
    assert (Graph.noNodes cfar == Graph.noNodes cfa') (F.pos stat) "Controllable action must be an instantaneous statement"
    return $ cfaTraceFile cfa' "action" cfa'
    
-- Check whether statement specifies a valid controllable action:
-- * Function, procedure, and controllable task calls only
-- * No fork statements
-- * No variable declarations
-- * No pause or stop statements
-- * No assert or assume
-- * No magic blocks
validateControllableStat :: (?spec::F.Spec) => F.Scope -> F.Statement -> Either String ()
validateControllableStat sc stat = do
    _ <- mapM (\(p,(_,m)) -> do assert (F.methCat m /= F.Task F.Uncontrollable) p "Uncontrollable task invocations are not allowed inside controllable actions"
                                assert (F.methCat m /= F.Task F.Invisible)      p "Invisible task invocations are not allowed inside controllable actions")
         $ F.statCallees sc stat
    _ <- F.mapStatM (\_ st -> case st of
                                       F.SVarDecl p _   -> err p "Variable declarations are not allowed inside controllable actions"
                                       F.SReturn  p _   -> err p "Return statements are not allowed inside controllable actions"
                                       F.SPar     p _   -> err p "Fork statements are not allowed inside controllable actions"
                                       F.SPause   p     -> err p "Pause statements are not allowed inside controllable actions"
                                       F.SWait    p _   -> err p "Wait statements are not allowed inside controllable actions"
                                       F.SStop    p     -> err p "Stop statements are not allowed inside controllable actions"
                                       F.SAssert  p _   -> err p "Assertions are not allowed inside controllable actions"
                                       F.SAssume  p _   -> err p "Assume statements are not allowed inside controllable actions"
                                       F.SMagic   p _ _ -> err p "Magic blocks are not allowed inside controllable actions"
                                       _                    -> return st) sc stat
    return ()

flatScopeToScope :: F.Spec -> F.Scope -> (F.Scope, F.IID)
flatScopeToScope inspec sc = 
    let ?spec = inspec in
    case sc of
         F.ScopeMethod   _ meth -> let (i, mname) = F.itreeParseName $ F.name meth
                                       tm = F.itreeTemplate i
                                       meth' = fromJust $ find ((== mname) . F.methName) $ F.tmAllMethod tm
                                   in (F.ScopeMethod tm meth', i)
         F.ScopeProcess  _ proc -> let (i, pname) = F.itreeParseName $ F.name proc
                                       tm = F.itreeTemplate i
                                       proc' = fromJust $ find ((== pname) . F.procName) $ F.tmAllProcess tm
                                   in (F.ScopeProcess tm proc', i)
         F.ScopeTemplate _      -> (F.ScopeTop, [])
