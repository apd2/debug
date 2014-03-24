{-# LANGUAGE ImplicitParams, RecordWildCards, ScopedTypeVariables, TupleSections #-}

module SourceView(SourceView.sourceViewNew, 
                  simulateTransition,
                  contTransToSource) where

import Data.Maybe
import Data.List
import Data.Tree
import Data.Tuple.Select
import Data.String.Utils
import qualified Data.Map                   as M
import Data.IORef
import qualified Data.Graph.Inductive.Graph as Graph
import qualified Graphics.UI.Gtk            as G
import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.ST
import Text.Parsec
import Control.Applicative
import qualified Text.PrettyPrint           as PP
import Debug.Trace

import Pos
import PP
import PID
import qualified Grammar
import Util hiding (name, trace)
import TSLUtil
import qualified DbgTypes      as D
import qualified IDE           as D
import qualified DbgAbstract   as D
import SourceViewTypes
import ISpec
import IExpr
import IVar
import IType
import CFA
import Inline
import Predicate
import Store
import SMTSolver
import qualified AbsSim            as CG
import qualified CFG               as CG
import qualified CuddExplicitDeref as C
import qualified Interface         as Abs
import qualified TermiteGame       as Abs
import CodeWin

import qualified NS                as F
import qualified Method            as F
import qualified InstTree          as F
import qualified Template          as F
import qualified TemplateOps       as F
import qualified ExprInline        as F
import qualified Spec              as F
import qualified Expr              as F
import qualified ExprFlatten       as F
import qualified ExprOps           as F
import qualified ExprValidate      as F
import qualified Statement         as F
import qualified StatementOps      as F
import qualified StatementValidate as F
import qualified StatementInline   as F
import qualified Name              as F
import qualified Type              as F
import qualified Pos               as F
import qualified TypeOps           as F

--------------------------------------------------------------
-- Constants
--------------------------------------------------------------

colorCont  = "#80c080"
colorUCont = "#8080c0"

--------------------------------------------------------------
-- Types
--------------------------------------------------------------


data ProcStackFrame = FrameRegular {frScope::F.Scope, frLoc::Loc}
                    | FrameMagic   {frScope::F.Scope, frLoc::Loc, frCFA::CFA}

isFrameMagic (FrameMagic _ _ _) = True
isFrameMagic _                  = False

instance PP ProcStackFrame where
    pp f = (if' (isFrameMagic f) (PP.text "(magic)") PP.empty) PP.<+> (PP.text $ show $ frScope f) PP.<> PP.char ':' PP.<+> (PP.text $ show $ frLoc f)

type ProcStack = [ProcStackFrame]

instance PP ProcStack where
    pp stack = PP.vcat $ map pp stack

stackToProcStack :: Stack -> ProcStack
stackToProcStack frames = map (\(Frame sc loc) -> FrameRegular sc loc) frames

-- Process stack, including magic frames
newtype EProcStack = EProcStack ProcStack

stackFrames :: EProcStack -> ProcStack
stackFrames (EProcStack fs) = fs

instance PP EProcStack where
    pp (EProcStack stack) = pp stack

instance Show EProcStack where
    show = PP.render . pp

data TraceEntry = TraceEntry {
    teStack :: EProcStack,
    teStore :: Store
}

instance PP TraceEntry where
    pp (TraceEntry stack _) = pp stack

type Trace = [TraceEntry]

instance PP Trace where
    pp tr = PP.vcat $ PP.punctuate (PP.char '\n') $ mapIdx (\e i -> pp i PP.<> PP.char ':' PP.$$ pp e) tr

showTrace :: Trace -> String
showTrace = PP.render . pp

data SourceView c a u = SourceView {
    svModel          :: D.RModel c a Store SVStore,
    svSpec           :: Spec,
    svInputSpec      :: F.Spec,
    svFlatSpec       :: F.Spec,
    svAbsVars        :: M.Map String AbsVar,
    svState          :: D.State a SVStore,          -- current state set via view callback
    svTmp            :: Store,                      -- temporary variables store
    svSolver         :: SMTSolver,

    -- Command buttons
    svSaveAllButton  :: G.ToolButton,
    svStepButton     :: G.ToolButton,
    svRunButton      :: G.ToolButton,
    svMagicButton    :: G.ToolButton,               -- generate code automatically
    svCodeGenButton  :: G.ToolButton,

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

    -- Code widget
    svCodeWin        :: RCodeWin,                   -- code widget
    svInprogLab      :: G.Label,                    -- transition in progress

    -- Resolve view
    svResolveView    :: G.TreeView,
    svResolveStore   :: G.TreeStore Expr,           -- tmp variables in the scope of the current expression
    svAutoResolve    :: Bool,                       -- resolve non-determinism automatically
    svAutoResolveTog :: G.CheckButton,              -- toggle auto-resolve mode button

    -- Stuff used for code generation
    svSTDdManager    :: C.STDdManager RealWorld u,
    svAbsDB          :: Abs.DB RealWorld u AbsVar AbsVar,
    svRefineDyn      :: Abs.RefineDynamic RealWorld u,
    svRefineStat     :: Abs.RefineStatic RealWorld u,
    svCompiledMBs    :: [(Pos, String)],             -- magic blocks completely filled with code
    svLab            :: Abs.Lab RealWorld u,
    svReachable      :: Maybe (C.DDNode RealWorld u) -- reachable state computed by simulating the game
}

type RSourceView c a u = IORef (SourceView c a u)

sourceViewEmpty = SourceView { svModel          = error "SourceView: svModel undefined"
                             , svInputSpec      = error "SourceView: svInputSpec undefined"
                             , svFlatSpec       = error "SourceView: svFlatSpec undefined"
                             , svSpec           = error "SourceView: svSpec undefined" 
                             , svAbsVars        = error "SourceView: svAbsVars undefined"
                             , svState          = error "SourceView: svState undefined"
                             , svTmp            = error "SourceView: svTmp undefined"
                             , svSolver         = error "SourceView: svSolver undefined"
                             , svSaveAllButton  = error "SourceView: svSaveAllButton undefined"
                             , svStepButton     = error "SourceView: svStepButton undefined"
                             , svRunButton      = error "SourceView: svRunButton undefined"
                             , svMagicButton    = error "SourceView: svMagicButton undefined"
                             , svCodeGenButton  = error "SourceView: svCodeGenButton undefined"
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
                             , svCodeWin        = error "SourceView: svCodeWin undefined"
                             , svInprogLab      = error "SourceView: svInprogLab undefined"
                             , svResolveView    = error "SourceView: svResolveView undefined"
                             , svResolveStore   = error "SourceView: svResolveStore undefined"
                             , svAutoResolve    = True
                             , svAutoResolveTog = error "SourceView: svAutoResolveTog undefined"
                             , svSTDdManager    = error "SourceView: svSTDdManager undefined"
                             , svAbsDB          = error "SourceView: svAbsDB undefined"
                             , svRefineDyn      = error "SourceView: svRefineDyn undefined"
                             , svRefineStat     = error "SourceView: svRefineStat undefined"
                             , svCompiledMBs    = []
                             , svLab            = error "SourceView: svLab undefined"
                             , svReachable      = Nothing
                             }


--------------------------------------------------------------
-- View callbacks
--------------------------------------------------------------

sourceViewNew :: (D.Rel c v a s) 
              => F.Spec 
              -> F.Spec 
              -> Spec 
              -> M.Map String AbsVar 
              -> SMTSolver 
              -> C.STDdManager RealWorld u
              -> Abs.RefineInfo RealWorld u AbsVar AbsVar st
              -> D.RModel c a Store SVStore 
              -> IO (D.View a Store SVStore)
sourceViewNew inspec flatspec spec absvars solver m Abs.RefineInfo{..} rmodel = do

    ref <- newIORef $ sourceViewEmpty { svModel          = rmodel
                                      , svInputSpec      = inspec
                                      , svFlatSpec       = flatspec
                                      , svSpec           = specInlineWirePrefix spec
                                      , svAbsVars        = absvars
                                      , svSolver         = solver
                                      , svSTDdManager    = m
                                      , svAbsDB          = db
                                      , svRefineDyn      = rd
                                      , svRefineStat     = rs
                                      , svLab            = lp
                                      }

    vbox <- G.vBoxNew False 0
    G.widgetShow vbox

    -- toolbar at the top
    tbar <- G.toolbarNew
    G.boxPackStart vbox tbar G.PackNatural 0
    G.widgetShow tbar

    -- File operations
    butsaveall <- G.toolButtonNewFromStock G.stockSave
    G.set butsaveall [G.widgetTooltipText G.:= Just "Save all files"]
    _ <- G.onToolButtonClicked butsaveall (saveAll ref)
    G.widgetShow butsaveall
    G.toolbarInsert tbar butsaveall (-1)

    sep0 <- G.separatorToolItemNew
    G.widgetShow sep0
    G.toolbarInsert tbar sep0 (-1)

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

    bmagic <- G.toolButtonNewFromStock G.stockEdit
    G.set bmagic [G.widgetTooltipText G.:= Just "Magic!"]
    _ <- G.onToolButtonClicked bmagic (autogen ref)
    G.widgetShow bmagic
    G.toolbarInsert tbar bmagic (-1)    

    bcg <- G.toolButtonNewFromStock G.stockExecute
    G.set bcg [G.widgetTooltipText G.:= Just "Generate code"]
    _ <- G.onToolButtonClicked bcg (codegen ref)
    G.widgetShow bcg
    G.toolbarInsert tbar bcg (-1)    


    modifyIORef ref (\sv -> sv { svRunButton     = butrun
                               , svSaveAllButton = butsaveall
                               , svStepButton    = butstep
                               , svMagicButton   = bmagic
                               , svCodeGenButton = bcg})

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

    -- code widget on the left
    src <- sourceWindowCreate ref
    D.panelsAppend hpanes src ""

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

    return $ D.View { D.viewName      = "Source"
                    , D.viewDefAlign  = D.AlignCenter
                    , D.viewShow      = return ()
                    , D.viewHide      = return ()
                    , D.viewGetWidget = return $ G.toWidget vbox
                    , D.viewQuit      = quit ref
                    , D.viewCB        = cb
                    }
    

sourceViewStateSelected :: (D.Rel c v a s) => RSourceView c a u -> Maybe (D.State a SVStore) -> IO ()
sourceViewStateSelected ref Nothing                                = disable ref
sourceViewStateSelected ref (Just s) | (not $ D.isConcreteState s) = disable ref
                                     | otherwise                   = do
    putStrLn $ "sourceViewStateSelected: store: " ++ (show $ D.sConcrete s)
    modifyIORef ref (\sv -> sv { svState    = s
                               , svTmp      = SStruct M.empty (svSpec sv)})
    processSelectorChooseUniqueEnabled ref
    reset ref
    putStrLn "sourceViewStateSelected done"

sourceViewTransitionSelected :: (D.Rel c v a s) => RSourceView c a u -> D.Transition a Store SVStore -> IO ()
sourceViewTransitionSelected ref tran | (not $ D.isConcreteTransition tran) = disable ref
                                      | otherwise                           = do
    putStrLn "sourceViewTransitionSelected"
    modifyIORef ref (\sv -> sv { svState    = D.tranFrom tran
                               , svTmp      = fromJust $ D.tranConcreteLabel tran})
    processSelectorChooseUniqueEnabled ref
    reset ref
    putStrLn "sourceViewTransitionSelected done"

--------------------------------------------------------------
-- Actions
--------------------------------------------------------------

stepAction :: (D.Rel c v a s) => RSourceView c a u -> IO ()
stepAction ref = do 
    sv <- readIORef ref
    -- sync PID
    let sv0 = maybeSetLCont $ setLPID (svPID sv) sv
    (msv1, entered) <- maybeEnterMB sv0
    -- don't make another step if we entered MB
    let msv' = maybe Nothing (\sv1 -> if' entered (Just sv1) (step sv1)) msv1 
    when (isJust msv') $ do writeIORef ref $ fromJust msv'
                            maybeExitMB ref 
                            sv2 <- readIORef ref                           
                            when (currentDelay sv2) $ makeTransition ref
                            updateDisplays ref

-- Execute one statement
step :: SourceView c a u -> Maybe (SourceView c a u)
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


runAction :: (D.Rel c v a s) => RSourceView c a u -> IO ()
runAction ref = do
    sv <- readIORef ref
    -- sync PID
    let sv0 = maybeSetLCont $ setLPID (svPID sv) sv
    (msv1, _) <- maybeEnterMB sv0
    let msv' = maybe Nothing run msv1
    case msv' of
         Nothing  -> return ()
         Just sv' -> do writeIORef ref sv'
                        maybeExitMB ref
                        sv2 <- readIORef ref
                        when (currentDelay sv2) $ makeTransition ref
                        updateDisplays ref
    

-- run until pause or nondeterministic choice
run :: SourceView c a u -> Maybe (SourceView c a u)
run sv = case step sv of
              Nothing  -> Nothing
              Just sv' -> if currentDelay sv' 
                             then Just sv'
                             else case run sv' of
                                       Nothing   -> Just sv'
                                       Just sv'' -> Just sv''

--exitMagicBlock :: (D.Rel c v a s) => RSourceView c a u -> IO ()
--exitMagicBlock ref = do
--    modifyIORef ref (\sv -> modifyCurrentStore sv (\st0 -> storeSet st0 mkMagicVar (Just $ SVal $ BoolVal False)))
--    makeTransition ref

-- simulate transition without GUI 
simulateTransition :: F.Spec -> Spec -> M.Map String AbsVar -> Store -> Store -> Maybe Store
simulateTransition flatspec spec absvars st lab =
    let spec' = specInlineWirePrefix spec
        -- create enough of source view to call run
        sv0 :: SourceView () () ()
        sv0 = sourceViewEmpty { svSpec       = spec'
                              , svFlatSpec   = flatspec
                              , svAbsVars    = absvars
                              , svState      = D.State { sAbstract = error "simulateTransition: sAbstract is undefined"
                                                       , sConcrete = Just $ (SVStore st [], error "simulateTransition: untracked is undefined")}
                              , svTmp        = lab
                              , svTracePos   = 0
                              , svStackFrame = 0
                              }
        pid = if storeEvalBool lab mkContLVar
                 then -- find process currently inside a magic block
                      case findProcInsideMagic sv0 of 
                           Just p -> p
                           _      -> error $ "simulateTransition no process inside a magic block"
                 else parsePIDEnumerator $ storeEvalEnum lab mkPIDLVar

        sv1 = if storeEvalBool lab mkContLVar
                 then -- execute controllable CFA
                      sv0 { svPID   = pid
                          , svTrace = [TraceEntry { teStore = storeUnion st lab
                                                  , teStack = EProcStack [FrameMagic F.ScopeTop cfaInitLoc (specCAct spec')]}]}
                 else -- execute uncontrollable process from its current location
                      sv0 { svPID   = pid
                          , svTrace = [TraceEntry { teStore = storeUnion st lab 
                                                  , teStack = EProcStack $ stackFromStore sv0 st pid}]} 
        msv2 = run sv1 
        mstore2 = case msv2 of
                       Nothing  -> trace ("simulateTransition: msv2 = Nothing") Nothing
                       Just sv2 -> if not $ currentDelay sv2
                                      then Nothing
                                      else Just $ applyExplicitUpdates sv2
    in trace ("simulateTransition\nlabel: " ++ show lab ++ "\nstate: " ++ show st)
       $ trace ("simulateTransitions returns " ++ show mstore2) 
       $ mstore2

saveAll :: RSourceView c a u -> IO ()
saveAll ref = do
    SourceView{..} <- readIORef ref
    codeWinSaveAll svCodeWin

contTransToSource :: F.Spec -> F.Spec -> Spec -> D.Transition a Store SVStore -> Maybe String
contTransToSource inspec flatspec spec D.Transition{..} = do
    iid <- findActiveMagicBlock flatspec spec (sstStore $ fst $ fromJust $ D.sConcrete tranFrom)
    act <- transitionToAction inspec flatspec (storeUnion (sstStore $ fst $ fromJust $ D.sConcrete tranTo) (fromJust tranConcreteLabel))
    doc <- ppContAction inspec iid act
    return $ PP.render doc

quit :: RSourceView c a u -> IO Bool 
quit ref = do
    SourceView{..} <- readIORef ref
    fs <- codeWinModifiedFiles svCodeWin
    case fs of
         [] -> return True
         _  -> saveQuitDialog ref fs

saveQuitDialog :: RSourceView c a u -> [String] -> IO Bool
saveQuitDialog ref fs = do
    SourceView{..} <- readIORef ref
    g <- G.messageDialogNew Nothing [] G.MessageQuestion G.ButtonsNone 
         $ "Save changes to the following files?\n" 
         ++ (intercalate "\n" $ fs) 

    _ <- G.dialogAddButton g "Yes"    G.ResponseYes
    _ <- G.dialogAddButton g "No"     G.ResponseNo
    _ <- G.dialogAddButton g "Cancel" G.ResponseCancel
    resp <- G.dialogRun g
    ret <- case resp of
                G.ResponseYes    -> do codeWinSaveAll svCodeWin
                                       return True
                G.ResponseNo     -> return True
                G.ResponseCancel -> return False
                G.ResponseNone   -> return False
    G.widgetDestroy g
    return ret

--------------------------------------------------------------
-- GUI components
--------------------------------------------------------------

-- Process selector --
processSelectorCreate :: RSourceView c a u -> IO G.Widget
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

processSelectorChanged :: RSourceView c a u -> IO ()
processSelectorChanged ref = do
    sv <- readIORef ref
    miter <- G.comboBoxGetActiveIter $ svProcessCombo sv
    when (isJust miter) $ 
        do path <- G.treeModelGetPath (svProcessStore sv) (fromJust miter)
           (pid, _) <- G.treeStoreGetValue (svProcessStore sv) path
           modifyIORef ref (\_sv -> _sv{svPID = pid})
           --cfaShow (specGetCFA (svSpec sv) pid Nothing) (pidToName pid)
           reset ref


pidtree :: SourceView c a u -> Forest PrID 
pidtree sv = map (\p -> procTree (PrID (procName p) []) p) (specProc $ svSpec sv)
    where procTree pid p = Node { rootLabel = pid
                                , subForest = map (\p' -> procTree (childPID pid (procName p')) p') (procChildren p)}

processSelectorInit :: RSourceView c a u -> IO ()
processSelectorInit ref = do
    sv <- readIORef ref
    let store = svProcessStore sv

    -- build store
    G.treeStoreClear store
    G.treeStoreInsertForest store [] 0 (map (fmap (,False)) $ pidtree sv)

processSelectorUpdate :: RSourceView c a u -> IO ()
processSelectorUpdate ref = do
    sv <- readIORef ref
    let store = svProcessStore sv
        combo = svProcessCombo sv
    myTreeModelForeach store (\iter -> do sv'      <- readIORef ref
                                          path     <- G.treeModelGetPath store iter
                                          (pid, _) <- G.treeStoreGetValue store path
                                          en       <- isProcEnabled sv' pid
                                          G.treeStoreSetValue store path (pid, en))
    miter <- G.comboBoxGetActiveIter combo
    when (isNothing miter) $ do miter' <- G.treeModelGetIter store [0]
                                G.comboBoxSetActiveIter combo (fromJust miter')
    G.widgetSetSensitive combo True

-- Check if there's only one enabled process in the current state
-- and, if yes, select this process.
processSelectorChooseUniqueEnabled :: RSourceView c a u -> IO ()
processSelectorChooseUniqueEnabled ref = do
    sv <- readIORef ref
    enpids <- filterM (isProcEnabled sv)
              $ concatMap flatten $ pidtree sv
    when (length enpids == 1) $ processSelectorSelectPID ref (head enpids)

processSelectorSelectPID :: RSourceView c a u -> PrID -> IO ()
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


processSelectorDisable :: RSourceView c a u -> IO ()
processSelectorDisable ref = do
    combo <- getIORef svProcessCombo ref
    G.widgetSetSensitive combo False

-- Stack --
stackViewCreate :: RSourceView c a u -> IO G.Widget
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
                     G.set rend [G.cellTextMarkup G.:= Just $ G.escapeMarkup $ show $ frScope frame])
    _ <- G.treeViewAppendColumn view col

    modifyIORef ref (\sv -> sv{svStackView = view, svStackStore = store, svStackFrame = 0})
    G.treeViewSetModel view store
    _ <- G.on view G.rowActivated (stackViewFrameSelected ref)
    panel <- D.framePanelNew (G.toWidget view) "Stack" (return ())
    D.panelGetWidget panel

stackViewFrameSelected :: RSourceView c a u -> G.TreePath -> G.TreeViewColumn -> IO ()
stackViewFrameSelected ref (idx:_) _ = do
    modifyIORef ref (\sv -> sv{svStackFrame = idx})
    sourceWindowUpdate ref
    watchUpdate ref

stackViewUpdate :: RSourceView c a u -> IO ()
stackViewUpdate ref = do
    sv <- readIORef ref
    let store = svStackStore sv
    G.listStoreClear store
    let frames = currentStackFrames sv
    _ <- mapM (G.listStoreAppend store) frames
    return ()

stackViewDisable :: RSourceView c a u -> IO ()
stackViewDisable ref = do
    sv <- readIORef ref
    G.listStoreClear $ svStackStore sv

-- Trace --

-- indices of visible states in the trace
svTraceVisible :: SourceView c a u -> [Int]
svTraceVisible sv = 
    filter (\i -> case locAct (getLocLabel sv i) of
                       ActNone -> False
                       _       -> True)
    $ [0..length (svTrace sv) - 1]

traceViewCreate :: RSourceView c a u -> IO G.Widget
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
                     G.set rend [G.cellTextMarkup G.:= Just $ "<span weight=\"" ++ (if idx == svTracePos sv then "bold" else "normal") ++ "\">" ++ (G.escapeMarkup txt) ++ "</span>"])
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

tracePosChanged :: RSourceView c a u -> IO ()
tracePosChanged ref = do
    sv <- readIORef ref
    miter <- G.comboBoxGetActiveIter $ svTraceCombo sv
    when (isJust miter) $ 
        do let storeidx = G.listStoreIterToIndex $ fromJust miter
           p <- G.listStoreGetValue (svTraceStore sv) storeidx
           -- only call updateDisplays if this is not a recursive call from updateDisplays
           when (p /= svTracePos sv) $ do writeIORef ref (traceSetPos sv p)
                                          updateDisplays ref


traceViewDisable :: RSourceView c a u -> IO ()
traceViewDisable ref = do
    sv <- readIORef ref
    G.listStoreClear $ svTraceStore sv
    G.widgetSetSensitive (svTraceUndo sv) False
    G.widgetSetSensitive (svTraceRedo sv) False
    G.widgetSetSensitive (svTraceCombo sv) False

traceViewUpdate :: RSourceView c a u -> IO ()
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


traceAppend :: SourceView c a u -> Store -> EProcStack -> SourceView c a u
traceAppend sv store stack = sv {svTrace = tr, svTracePos = p}
    where tr = take (svTracePos sv + 1) (svTrace sv) ++ [TraceEntry stack store]
          p  = length tr - 1

traceSetPos :: SourceView c a u -> Int -> SourceView c a u
traceSetPos sv i | (i >= (length $ svTrace sv)) || (i < 0) = sv
                 | otherwise = sv {svTracePos = i, svStackFrame = 0}


-- Watch --

watchCreate :: RSourceView c a u -> IO G.Widget
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
                     G.set valrend [G.cellTextMarkup G.:= Just $ G.escapeMarkup 
                                                          $ case mexp of 
                                                                 Nothing  -> ""
                                                                 Just e -> case storeEvalStr svInputSpec svFlatSpec svSpec
                                                                                             (currentStore sv) 
                                                                                             (Just svPID) 
                                                                                             (frScope $ currentStackFrames sv !! svStackFrame) e of
                                                                                Left er -> er
                                                                                Right v -> let str = show v in
                                                                                           if' (length str > 256) ((take 256 str) ++ "...") str])
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

watchEditingStarted :: RSourceView c a u ->  G.Widget -> G.TreePath -> IO ()
watchEditingStarted ref w path = do
    let entry = G.castToEntry w
    store <- getIORef svWatchStore ref
    val <- G.listStoreGetValue store (head path)
    when (isNothing val) $ G.entrySetText entry ""

watchChanged :: RSourceView c a u -> G.TreePath -> String -> IO ()
watchChanged ref path val = do
    store <- getIORef svWatchStore ref
    G.listStoreSetValue store (head path) (Just val)
    watchUpdate ref 

watchDelete :: RSourceView c a u -> IO ()
watchDelete ref = do
    sv <- readIORef ref
    (idx, _) <- G.treeViewGetCursor (svWatchView sv)
    when (not $ null idx) $ G.listStoreRemove (svWatchStore sv) (head idx)
    watchUpdate ref

watchUpdate :: RSourceView c a u -> IO ()
watchUpdate ref = do
    store <- getIORef svWatchStore ref
    items <- G.listStoreToList store
    -- make sure that there is an empty slot in the end
    let items' = (filter isJust items) ++ [Nothing]
    -- refill the list to force watch update
    G.listStoreClear store
    _ <- mapM (G.listStoreAppend store) items'
    return ()

watchDisable :: RSourceView c a u -> IO ()
watchDisable _ = return ()


-- Code widget --
sourceWindowCreate :: (D.Rel c v a s) => RSourceView c a u -> IO G.Widget
sourceWindowCreate ref = do
    vbox <- G.vBoxNew False 0
    G.widgetShow vbox
    -- Source window at the top

    spec <- getIORef svInputSpec ref
    code <- codeWinNew spec
    codewid <- codeWinWidget code
    codepos <- codeWinPos code
    G.boxPackStart vbox codewid G.PackGrow 0
    -- Status bar at the bottom
    sbar <- G.hBoxNew True 0
    G.widgetShow sbar
    G.boxPackStart vbox sbar G.PackNatural 0
    -- transition-in-progress label
    lprog <- G.labelNew Nothing
    G.widgetShow lprog
    G.boxPackStart sbar lprog G.PackGrow 0
    G.boxPackEnd sbar codepos G.PackNatural 0
  
    modifyIORef ref (\sv -> sv { svCodeWin   = code
                               , svInprogLab = lprog})

    return $ G.toWidget vbox

sourceWindowUpdate :: RSourceView c a u -> IO ()
sourceWindowUpdate ref = do
    putStrLn "sourceWindowUpdate"
    sv@SourceView{..} <- readIORef ref
    -- activate magic block if necessary
    codeWinMBActivate svCodeWin $ stackGetMBID sv $ currentStack sv
    -- highlight statement
    let cfa = cfaAtFrame sv svStackFrame
        frames = currentStackFrames sv
        loc = frLoc $ frames !! svStackFrame
        mmbid = stackGetMBID sv $ EProcStack $ drop svStackFrame frames
        color = maybe colorUCont (\_ -> colorCont) mmbid
        -- if we called a method from inside MB, current syntactic scope
        -- is not inside MB anymore
        mmbid' = if' (isFrameMagic $ head frames) mmbid Nothing
    case locAct $ cfaLocLabel loc cfa of
         ActNone   -> codeWinClearSelection svCodeWin
         ActExpr e -> codeWinSetSelection svCodeWin mmbid' (F.pos e) color
         ActStat s -> codeWinSetSelection svCodeWin mmbid' (F.pos s) color
    G.labelSetMarkup svInprogLab $ if svTracePos == 0
                                      then if currentError sv
                                              then "<span color=\"red\" weight=\"bold\">ERROR</span>"
                                              else "<span weight=\"bold\">PAUSE</span>" 
                                      else ""

sourceWindowDisable :: RSourceView c a u -> IO ()
sourceWindowDisable ref = do
    sv <- readIORef ref
    G.labelSetText (svInprogLab sv) ""
    codeWinClearSelection (svCodeWin sv)

-- Command buttons --
commandButtonsUpdate :: RSourceView c a u -> IO ()
commandButtonsUpdate ref = do
    sv <- readIORef ref
    -- enable step and run buttons if we are not at a pause location or
    -- if we are in a pause location and the wait condition is true
    let ?spec = svSpec sv
    pen <- isProcEnabled sv (svPID sv)
    let en = -- current process must be enabled and ...
             pen
             && 
             -- ... non-determinism must be resolved
             -- (all scalar tmp variables that affect the next transition must be assigned)
             (all isJust           
              $ map (storeTryEval (currentStore sv))
              $ filter isScalar
              $ concatMap flatten 
              $ currentTmpExprTree sv)
    G.widgetSetSensitive (svSaveAllButton sv) True
    G.widgetSetSensitive (svStepButton sv)    en
    G.widgetSetSensitive (svRunButton sv)     en
    G.widgetSetSensitive (svCodeGenButton sv) True
    G.widgetSetSensitive (svMagicButton sv) $  (isMBLabel $ currentLocLabel sv)                     -- we're at a magic block entrance
                                            && (currentMagic sv)
                                            && (svTracePos sv == 0)                                 -- there is no transition in progress

commandButtonsDisable :: RSourceView c a u -> IO ()
commandButtonsDisable ref = do
    sv <- readIORef ref
    -- disable command buttons
    G.widgetSetSensitive (svSaveAllButton sv) False
    G.widgetSetSensitive (svStepButton sv)    False
    G.widgetSetSensitive (svRunButton sv)     False
    G.widgetSetSensitive (svMagicButton sv)   False
    G.widgetSetSensitive (svCodeGenButton sv) False

-- Resolve --

resolveViewCreate :: RSourceView c a u -> IO G.Widget
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
                     G.set namerend [G.cellTextMarkup G.:= Just $ G.escapeMarkup $ if' hl ("<span background=\"red\">" ++ show e ++ "</span>") (show e)])
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
                     G.set textrend [ G.cellVisible      G.:= isInt e
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
                     G.set combrend [ G.cellVisible        G.:= isScalar e && not (isInt e)
                                    , G.cellComboTextModel G.:= (tmodel, colid)
                                    , G.cellTextEditable   G.:= True
                                    , G.cellComboHasEntry  G.:= False
                                    , G.cellText           G.:= case storeTryEval (currentStore sv) e of
                                                                     Nothing -> "*"
                                                                     Just v  -> show v])
    _ <- G.on combrend G.edited (textAsnChanged ref) 

    _ <- G.treeViewAppendColumn view valcol
    modifyIORef ref (\sv -> sv { svResolveView    = view
                               , svResolveStore   = store
                               , svAutoResolveTog = bauto})
    panel <- D.framePanelNew (G.toWidget vbox) "Resolve non-determinism" (return ())
    D.panelGetWidget panel

toggleAutoResolve :: RSourceView c a u -> IO ()
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

textAsnChanged :: RSourceView c a u -> G.TreePath -> String -> IO ()
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

resolveViewUpdate :: RSourceView c a u -> IO ()
resolveViewUpdate ref = do
    sv <- readIORef ref
    G.toggleButtonSetActive (svAutoResolveTog sv) (svAutoResolve sv)
    let exprs = currentTmpExprTree sv
        store = svResolveStore sv
    G.treeStoreClear store
    G.treeStoreInsertForest store [] 0 exprs
    G.treeViewExpandAll (svResolveView sv)

resolveViewDisable :: RSourceView c a u -> IO ()
resolveViewDisable ref = do
    sv <- readIORef ref
    G.treeStoreClear $ svResolveStore sv

autoResolve :: RSourceView c a u -> IO ()
autoResolve ref = do
     sv <- readIORef ref
     let ?spec = svSpec sv
     _ <- mapM (autoResolve1 ref)
          $ filter isScalar
          $ concatMap flatten 
          $ currentTmpExprTree sv
     return ()

autoResolve1 :: RSourceView c a u -> Expr -> IO ()
autoResolve1 ref e = do
    modifyIORef ref (\sv -> let ?spec = svSpec sv in
                            if isNothing $ storeTryEval (currentStore sv) e
                               then modifyCurrentStore sv (\s -> storeSet s e (Just $ SVal $ valDefault e))
                               else sv)
    
--runControllableCFA :: RSourceView c a u -> CFA -> IO ()
--runControllableCFA ref cfa = do
--    -- Push controllable cfa on the stack
--    modifyIORef ref (\sv -> let frames = currentStackFrames sv
--                                stack' = EProcStack $ (FrameMagic (frScope $ head frames) cfaInitLoc cfa) : frames
--                            in traceAppend sv (currentStore sv) stack')
--    updateDisplays ref

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
             (PP.parens $ PP.hcat $ PP.punctuate PP.comma $ map (\a -> if' (F.argDir a == F.ArgOut) (PP.char '_') (pp $ fromJust $ lookup (F.sname a) as)) $ F.methArg t)
    
-- Translate an abstract transition into a controllable action
-- cnstate - concrete state before the transition
transitionToAction :: F.Spec -> F.Spec -> Store -> Maybe ContAction
transitionToAction inspec flatspec cnstate = do
    let ?spec = inspec
    let tag = storeEvalEnum cnstate mkTagVar 
    let cont = storeEvalBool cnstate mkContLVar
    (if' (not cont) Nothing
     $ if' (tag == mkTagExit) (if' (not $ storeEvalBool cnstate mkMagicVar) (return ActExit) Nothing)
     $ do let flatmeth = let ?spec = flatspec in fromJust $ find ((== tag) . F.sname) $ F.tmMethod tmMain
          let (caIID, methname) = F.itreeParseName tag
              Just (F.ObjMethod tm caTask) = F.lookupGlobal ((F.Ident F.nopos "main"):caIID++[F.Ident nopos methname])
          let caArgs = map (\a -> (F.sname a, let ?scope = F.ScopeTemplate tm in storeToFExpr a
                                              $ storeEval cnstate 
                                              $ (EVar $ mkVarNameS (NSID Nothing (Just flatmeth)) $ F.sname a)))
                       $ filter ((== F.ArgIn) . F.argDir)
                       $ F.methArg caTask
          return ActCall{..})


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
                                        SStruct sfs _ = s
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
   let sv0 :: SourceView () () ()
       sv0 = sourceViewEmpty { svSpec       = spec
                             , svFlatSpec   = flatspec
                             , svState      = D.State { sAbstract = error "findMagicBlock: sAbstract is undefined"
                                                      , sConcrete = Just $ (SVStore st [], error "findMagicBlock: untracked is undefined")}
                             , svTracePos   = 0
                             , svStackFrame = 0
                             }
   in (fmap (\pid -> let stack = stackFromStore sv0 st pid
                         sc = frScope $ head stack
                         tm = case sc of
                                   F.ScopeProcess tm' _ -> tm'
                                   F.ScopeMethod  tm' _ -> tm'
                      in fst $ F.itreeParseName (F.sname tm) )) 
      $ findProcInsideMagic sv0


--------------------------------------------------------------
-- Private helpers
--------------------------------------------------------------

-- assumes: 
-- * we're at a magic block entrance
-- * there is no transition in progress
autogen :: (D.Rel c v a s) => RSourceView c a u -> IO ()
autogen ref = do
    -- make sure we're in controllable state
    sv@SourceView{..} <- readIORef ref
    let ActStat (F.SMagic p _) = locAct $ currentLocLabel sv
    -- request transition from oracle
    mtran <- D.modelAdviseTransition svModel
    -- translate transition to source
    txt <- maybe (return "/* no transition */")
                 (\t -> do mt' <- D.modelConcretiseTransition svModel t
                           return $ 
                            maybe "/* failed to concretise transition */"
                                  (maybe "/* failed to convert transition to code */" 
                                         (\s -> if s == "exit"
                                                   then "{}"
                                                   else s ++ ";\n" ++ replicate (sourceColumn (fst p) - 1) ' ' ++ "...")
                                   . contTransToSource svInputSpec svFlatSpec svSpec)
                                  mt')
                 mtran
    -- insert it into magic block
    mactive <- codeWinActiveMB svCodeWin
    let mbid = maybe (MBID p []) (\m -> mbidChild m $ currentLoc sv) mactive
    codeWinSetMBText svCodeWin mbid txt

-- Generate code for MB under cursor
codegen :: (D.Rel c v a s) => RSourceView c a u -> IO ()
codegen ref = do
    sv@SourceView{..} <- readIORef ref
    -- Locate MB under cursor or its parent MB (if the MB does not have its own region)
    mmb <- codeWinMBAtCursor svCodeWin
    case mmb of
         Nothing        -> D.showMessage svModel G.MessageError "No editable magic block under cursor"
         Just (mbid, p) -> do 
             -- Flatten MB by making it stale
             codeWinMBMakeStale svCodeWin mbid
             MBI mbi <- codeWinGetMB svCodeWin mbid
             mbtxt <- mbiGetRegionText svCodeWin mbi
             let (pid,_,sc) = fromJust $ specLookupMB svSpec (mbidPos mbid)
             -- Compile parent MB and locate the MB to be synthesised inside it.
             if mbtxt == "..."
                then doCodeGen ref mbid
                else case compileMB sv sc pid mbtxt of
                          Left e    -> D.showMessage svModel G.MessageError e
                          Right cfa -> do codeWinMBRefresh svCodeWin mbid cfa
                                          case cfaFindMBAtPos cfa p of
                                               Nothing  -> D.showMessage svModel G.MessageError "No magic block at this location"
                                               Just loc -> doCodeGen ref $ mbidChild mbid loc

-- Generate code for empty MB identified by the argument
doCodeGen :: (D.Rel c v a s) => RSourceView c a u -> MBID -> IO ()
doCodeGen ref mbid = do
    -- Simulate game
    ok <- reSimulate ref
    when ok $ doCodeGen' ref mbid

doCodeGen' :: (D.Rel c v a s) => RSourceView c a u -> MBID -> IO ()
doCodeGen' ref mbid@(MBID p locs) = do
    sv@SourceView{..} <- readIORef ref
    let (mbpid,_,mbsc) = fromJust $ specLookupMB svSpec p
    ctx <- D.modelCtx svModel
    -- Set of states at the outermost MB entry
    initset <- stToIO $ CG.restrictToMB svSpec svSTDdManager svAbsDB p (fromJust svReachable)
    -- Simulate nested MBs until reaching the target one
    mbd <- codeWinGetMB svCodeWin mbid
    minitset' <- simulateNestedMBs sv initset mbd locs
    strategy <- fromJust <$> D.modelStrategy svModel
    case minitset' of
         Nothing       -> D.showMessage svModel G.MessageError "Magic block is not reachable--cannot generate code"
         Just initset' -> do code <- stToIO $ do -- Generate code
                                 strategyst <- D.relToDDNode ctx strategy
                                 stp@CG.Step{..} <- CG.gen1Step svSpec svSTDdManager svRefineDyn svAbsDB (Abs.cont svRefineStat) svLab initset' strategyst
                                 C.deref svSTDdManager strategyst
                                 C.deref svSTDdManager initset'
                                 res <- CG.ppStep svInputSpec svFlatSpec svSpec mbpid svSTDdManager mbsc svAbsDB stp
                                 CG.derefStep svSTDdManager stp
                                 return res
                             codeWinSetMBText svCodeWin mbid $ PP.render code

-- Consumes the initset reference
simulateNestedMBs :: SourceView c a u -> C.DDNode RealWorld u -> MBDescr -> [Loc] -> IO (Maybe (C.DDNode RealWorld u))
simulateNestedMBs _                 initset _   []         = return $ Just initset
simulateNestedMBs sv@SourceView{..} initset mbd (loc:locs) = do
    minitset' <- stToIO $ do CG.simulateCFAAbstractToLoc svSpec svSTDdManager svRefineDyn svAbsDB (Abs.cont svRefineStat) svLab (mbCFA mbd) initset loc
    stToIO $ C.deref svSTDdManager initset
    case minitset' of
         Nothing       -> return Nothing
         Just initset' -> simulateNestedMBs sv initset' (fromJust $ lookupMB mbd [loc]) locs

reSimulate :: RSourceView c a u -> IO Bool
reSimulate ref = do
    sv@SourceView{..} <- readIORef ref
    -- Find and compile all complete magic blocks
    embs <- liftM sequence
            $ mapM (\(p,_,_) -> do let (pid,_,sc) = fromJust $ specLookupMB svSpec p
                                   txt <- codeWinGetAllMBText svCodeWin (MBID p [])
                                   case compileMB sv sc pid txt of
                                        Left  e   -> return $ Left  (p,e)
                                        Right cfa -> return $ Right (p,txt,cfa))
            $ specAllMBs svSpec
    case embs of
         Left (p,e) -> do D.showMessage svModel G.MessageError $ "Error compiling magic block at " ++ show p ++ ": " ++ e
                          return False
         Right mbs  -> let mbs'  = filter (null . cfaFindMBs . sel3) mbs
                           mbstxt = map (\(p, txt, _) -> (p,txt)) mbs'
                           mbscfa = map (\(p, _, cfa) -> (p,cfa)) mbs' in
                       if mbstxt == svCompiledMBs && isJust svReachable
                          then return True
                          else do reach <- stToIO $ do maybe (return ()) (C.deref svSTDdManager) svReachable
                                                       CG.simulateGameAbstract svSpec svSTDdManager svRefineDyn svAbsDB (Abs.cont svRefineStat) svLab mbscfa (Abs.init svRefineStat)
                                  writeIORef ref $ sv {svCompiledMBs = mbstxt, svReachable = Just reach}
                                  return True

-- If we are about to enter magic block, activate it.
maybeEnterMB :: SourceView c a u -> IO (Maybe (SourceView c a u), Bool)
maybeEnterMB sv = do
    let lab = currentLocLabel sv
    if' (isMBLabel lab && currentMagic sv)
        (do let ActStat (F.SMagic p _) = locAct lab
            liftM (,True) $ enterMB sv (p, currentLoc sv))
        (return (Just sv, False))

enterMB :: SourceView c a u -> (F.Pos, Loc) -> IO (Maybe (SourceView c a u))
enterMB sv@SourceView{..} (p,l) = do
    putStrLn "enterMB"
    mactive <- codeWinActiveMB svCodeWin
    let mbid = maybe (MBID p []) (\m -> mbidChild m l) mactive
    putStrLn $ "enterMB: mbid=" ++ show mbid
    MBI mbi <- codeWinGetMB svCodeWin mbid
    text <- mbiGetRegionText svCodeWin mbi
    let frames = currentStackFrames sv
        sc = frScope $ head frames
    if' (isMBICurrent mbi)
        (do codeWinMBActivate svCodeWin (Just mbid)
            return $ Just $ traceAppend sv (currentStore sv) (EProcStack $ (FrameMagic sc cfaInitLoc (mbiCFA mbi)):frames))
        (case compileMB sv sc svPID text of
              Left e    -> do D.showMessage svModel G.MessageError e
                              return Nothing
              Right cfa -> do codeWinMBRefresh svCodeWin mbid cfa
                              codeWinMBActivate svCodeWin (Just mbid)
                              return $ Just $ traceAppend sv (currentStore sv) (EProcStack $ (FrameMagic sc cfaInitLoc cfa):frames))

-- If we are about to exit magic block, deactivate it first.
maybeExitMB :: (D.Rel c v a s) => RSourceView c a u -> IO ()
maybeExitMB ref = do
    putStrLn "maybeExitMB"
    sv <- readIORef ref
    let fr:_ = currentStackFrames sv
    if isFrameMagic fr && isDeadendLoc (currentCFA sv) (currentLoc sv)
       then do exitMB ref
               maybeExitMB ref -- exit all nested MB's
       else return ()

exitMB :: (D.Rel c v a s) => RSourceView c a u -> IO ()
exitMB ref = do
    putStrLn "exitMB"
    sv0 <- readIORef ref
    let st0 = currentStore sv0
    Just mbid@(MBID _ ls) <- codeWinActiveMB $ svCodeWin sv0
    putStrLn $ "exitMB: mbid=" ++ show mbid
--    -- if we're about to exit an outermost MB, insert additional exit transition.
--    when (null ls) $ makeTransition ref
--    sv1 <- readIORef ref
    let frames = currentStackFrames sv0
        sv1 = trace ("frames=" ++ show (EProcStack frames)) $
              if null ls
                 then modifyCurrentStore sv0 (\_ -> storeSet st0 mkMagicVar (Just $ SVal $ BoolVal False))
                 else sv0
        sv2 = traceAppend sv1 (currentStore sv1) (EProcStack $ tail frames)
    -- pop the magic block from the stack
        sv3 = if null ls
                 then sv2
                 else fromJust $ step sv2 -- 
    codeWinMBActivate (svCodeWin sv3) $ stackGetMBID sv3 $ currentStack sv3
    writeIORef ref sv3


-- Given a snapshot of the store at a pause location, compute
-- process stack.
stackFromStore :: SourceView c a u -> Store -> PrID -> ProcStack
stackFromStore sv st pid = stackToProcStack (locStack lab)
    where cfa   = specGetCFA (svSpec sv) (EPIDProc pid)
          loc   = storeGetLoc st pid
          lab   = cfaLocLabel loc cfa

-- As above, but include MB stack
extStackFromStore :: SourceView c a u -> SVStore -> PrID -> IO EProcStack
extStackFromStore sv SVStore{..} pid = do
    cw <- readIORef $ svCodeWin sv
    let cfa  = specGetCFA (svSpec sv) (EPIDProc pid)
        loc  = storeGetLoc sstStore pid
        lab  = cfaLocLabel loc cfa
        mbst = case locAct lab of
                    ActStat (F.SMagic p _) -> mbStackToProcStack cw p sstMBStack
                    _                      -> []
    return $ EProcStack $ mbst ++ stackToProcStack (locStack lab)


mbStackToProcStack :: CodeWin -> F.Pos -> [MBFrame] -> [ProcStackFrame]
mbStackToProcStack cw p fs = mbStackToProcStack' [] cw (MBID p []) $ reverse fs

mbStackToProcStack' :: [ProcStackFrame] -> CodeWin -> MBID -> [MBFrame] -> [ProcStackFrame]
mbStackToProcStack' st0 _  _    []               = st0
mbStackToProcStack' st0 cw mbid (MBFrame{..}:fs) = 
    case cwLookupMB cw mbid of
         Nothing -> []
         Just mb -> if mbEpoch mb == mbfEpoch
                       then let -- compute CFA stack for the innermost MB
                                cfa = case mb of
                                           MBA mba -> mbaCFA mba
                                           MBI mbi -> mbiCFA mbi
                                lab = cfaLocLabel mbfLoc cfa
                                st' = stackToProcStack (locStack lab) 
                                sc  = frScope $ head st' 
                                st1 = FrameMagic sc mbfLoc cfa : st0 in
                            if' (null fs) 
                                ((init st') ++ st1) {- bottom of st' is the same as FrameMagic -}
                                (mbStackToProcStack' st1 cw (mbidChild mbid mbfLoc) fs)
                       else []


-- If the current process is inside MB, convert its stack to MB stack
-- Otherwise, leave MB stack unmodified
currentMBStack :: SourceView c a u -> IO [MBFrame]
currentMBStack sv | findProcInsideMagic sv == Just (svPID sv) = procStackToMBStack sv (currentStack sv)
                  | otherwise                                 = return $ sstMBStack $ fst $ fromJust $ D.sConcrete $ svState sv

procStackToMBStack :: SourceView c a u -> EProcStack -> IO [MBFrame]
procStackToMBStack sv stack@(EProcStack fs) = do
    case stackGetMBID sv stack of
         Nothing         -> return []
         Just (MBID p _) -> foldM (\mbst fr -> do let mbid = MBID p $ map mbfLoc $ reverse mbst
                                                  mb <- codeWinGetMB (svCodeWin sv) mbid
                                                  return $ MBFrame (mbEpoch mb) (frLoc fr) : mbst) []
                            $ filter isFrameMagic 
                            $ reverse fs
  
stackGetMBID :: SourceView c a u -> EProcStack -> Maybe MBID
stackGetMBID sv (EProcStack frames) = stackGetMBID' sv Nothing $ reverse frames

stackGetMBID' :: SourceView c a u -> Maybe MBID -> [ProcStackFrame] -> Maybe MBID
stackGetMBID' _  mmbid [_]        = mmbid
stackGetMBID' sv mmbid (f0:f1:fs) | isFrameMagic f1 = 
    maybe (let cfa = specGetCFA (svSpec sv) (EPIDProc $ svPID sv)
               ActStat (F.SMagic p _) = locAct $ cfaLocLabel (frLoc f0) cfa 
           in stackGetMBID' sv (Just $ MBID p []) (f1:fs))
          (\mbid -> stackGetMBID' sv (Just $ mbidChild mbid $ frLoc f0) (f1:fs))
          mmbid
                                  | otherwise       = stackGetMBID' sv mmbid (f1:fs)

stackGetCFA :: SourceView c a u -> PrID -> EProcStack -> CFA
stackGetCFA sv pid (EProcStack stack) = stackGetCFA' sv stack pid

stackGetCFA' :: SourceView c a u -> [ProcStackFrame] -> PrID -> CFA
stackGetCFA' sv []                     pid = specGetCFA (svSpec sv) (EPIDProc pid)
stackGetCFA' sv ((FrameRegular _ _):s) pid = stackGetCFA' sv s pid
stackGetCFA' _  ((FrameMagic{..}):_)   _   = frCFA

storeGetLoc :: Store -> PrID -> Loc
storeGetLoc s pid = pcEnumToLoc pc
    where pcvar = EVar $ mkPCVarName pid
          pc    = maybe (mkPCEnum pid cfaInitLoc) id $ storeTryEvalEnum s pcvar

-- Used to highlight enabled processes in process selector
isProcEnabled :: SourceView c a u -> PrID -> IO Bool
isProcEnabled sv pid = do
    let store@SVStore{..} = fst $ fromJust $ D.sConcrete $ svState sv
    stack@(EProcStack (frame:_)) <- extStackFromStore sv store pid
    let loc   = frLoc frame
        cfa   = stackGetCFA sv pid stack
        lab   = cfaLocLabel loc cfa
        -- cont  = storeEvalBool sstStore mkContVar
        cond  = case lab of
                     LPause _ _ _ c -> storeEvalBool sstStore c
                     LFinal _ _ _   -> (not $ null $ Graph.lsuc cfa loc) || isFrameMagic frame
                     _              -> True
    return $ case storeTryEvalBool (svTmp sv) mkContLVar of
                  Just False -> case fmap parsePIDEnumerator $ storeTryEvalEnum (svTmp sv) mkPIDLVar of
                                     Nothing   -> (not $ isControllableCode sv pid stack) && cond
                                     Just pid' -> pid == pid'
                  Just True  -> isControllableCode sv pid stack
                  Nothing    -> -- If the process is running uncontrollable code, then the enabled condition is
                                -- its wait condition
                                -- If the process is running controllable code, then it is enabled if its program
                                -- counter (at the top of the stack) is either inside a magic block or at a pause
                                -- location in a nested CFA whose wait condition is satisfied.
                                if' (isControllableCode sv pid stack) 
                                    (cond || (isMBLabel lab))
                                    cond


isProcControllableCode :: SourceView c a u -> PrID -> Bool
isProcControllableCode sv pid = isControllableCode sv pid (EProcStack stack)
    where
    store = sstStore $ fst $ fromJust $ D.sConcrete $ svState sv
    stack = stackFromStore sv store pid

findProcInsideMagic :: SourceView c a u -> Maybe PrID
findProcInsideMagic sv = find (isProcControllableCode sv)
                         $ concatMap flatten $ pidtree sv

-- True if process is running controllable code, i.e., is inside a top-level MB.
isControllableCode :: SourceView c a u -> PrID -> EProcStack -> Bool
isControllableCode sv pid (EProcStack frames) = isMBLoc cfa loc && storeEvalBool store mkMagicVar
    where
    store  = sstStore $ fst $ fromJust $ D.sConcrete $ svState sv
    pstack = reverse $ takeWhile (not . isFrameMagic) $ reverse frames
    cfa    = stackGetCFA sv pid (EProcStack pstack)
    loc    = frLoc $ head pstack

-- update all displays
updateDisplays :: RSourceView c a u -> IO ()
updateDisplays ref = do
    autores <- getIORef svAutoResolve ref
    when autores $ autoResolve ref
    commandButtonsUpdate ref
    stackViewUpdate      ref
    traceViewUpdate      ref
    watchUpdate          ref
    sourceWindowUpdate   ref
    resolveViewUpdate    ref

-- Reset all components
reset :: RSourceView c a u -> IO ()
reset ref = do
    -- processSelectorUpdate expects initialised store
    sv0 <- readIORef ref
    let store = storeUnion (sstStore $ fst $ fromJust $ D.sConcrete $ svState sv0) (svTmp sv0)
        tr = [TraceEntry { teStack = error "teStack is undefined"
                         , teStore = store }] 
    writeIORef ref (sv0 {svTrace = tr, svTracePos = 0, svStackFrame = 0})
    processSelectorUpdate ref
    
    sv1 <- readIORef ref
    stack <- extStackFromStore sv1 (fst $ fromJust $ D.sConcrete $ svState sv1) (svPID sv1)
    -- initialise trace
    let tr' = [TraceEntry { teStack = stack
                          , teStore = store}]
    writeIORef ref sv1{svTrace = tr', svTracePos = 0, svStackFrame = 0}
    updateDisplays ref

-- Disable all controls
disable :: RSourceView c a u -> IO ()
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

-- Access current location in the trace 

currentCFA :: SourceView c a u -> CFA
currentCFA sv = getCFA sv (svTracePos sv)

currentLoc :: SourceView c a u -> Loc
currentLoc sv = getLoc sv (svTracePos sv)

currentLocLabel :: SourceView c a u -> LocLabel
currentLocLabel sv = getLocLabel sv (svTracePos sv)

currentDelay :: SourceView c a u -> Bool
currentDelay sv = getDelay sv (svTracePos sv)

currentStore :: SourceView c a u -> Store
currentStore sv = getStore sv (svTracePos sv)

initialStore :: SourceView c a u -> Store
initialStore sv = getStore sv 0

modifyStore :: SourceView c a u -> Int -> (Store -> Store) -> SourceView c a u
modifyStore sv idx f = sv {svTrace = tr'}
    where tr     = svTrace sv
          entry  = tr !! idx
          entry' = entry {teStore = (f $ teStore entry)}
          tr'    = take idx tr ++ [entry'] ++ drop (idx+1) tr

modifyCurrentStore :: SourceView c a u -> (Store -> Store) -> SourceView c a u
modifyCurrentStore sv f = modifyStore sv (svTracePos sv) f

currentStack :: SourceView c a u -> EProcStack
currentStack sv = getStack sv (svTracePos sv)

currentStackFrames = stackFrames . currentStack

currentTmpExprTree :: SourceView c a u -> Forest Expr
currentTmpExprTree sv = getTmpExprTree sv (svTracePos sv)

--currentControllable :: SourceView c a u -> Bool
--currentControllable sv = storeEvalBool (currentStore sv) mkContVar

currentMagic :: SourceView c a u -> Bool
currentMagic sv = storeEvalBool (currentStore sv) mkMagicVar

currentError :: SourceView c a u -> Bool
currentError sv = storeEvalBool (currentStore sv) mkErrVar

cfaAtFrame :: SourceView c a u -> Int -> CFA
cfaAtFrame sv frame = let frames = currentStackFrames sv 
                      in stackGetCFA sv (svPID sv) $ EProcStack $ drop frame frames

-- Access arbitrary location in the trace 

getCFA :: SourceView c a u -> Int -> CFA
getCFA sv p = stackGetCFA sv (svPID sv) $ getStack sv p

getLoc :: SourceView c a u -> Int -> Loc
getLoc sv p = frLoc $ head $ stackFrames $ getStack sv p

getLocLabel :: SourceView c a u -> Int -> LocLabel
getLocLabel sv p = cfaLocLabel (getLoc sv p) (getCFA sv p)

getDelay :: SourceView c a u -> Int -> Bool
getDelay sv p = (isDelayLabel $ getLocLabel sv p) &&
                -- initial location of magic block CFA is not considered delay location
                (not $ (getLoc sv p == cfaInitLoc) && (isJust $ stackGetMBID sv $ getStack sv p))
                
getStore :: SourceView c a u -> Int -> Store
getStore sv p | p >= (length $ svTrace sv) = SStruct M.empty (svSpec sv)
              | otherwise                  = teStore $ svTrace sv !! p

getStack :: SourceView c a u -> Int -> EProcStack
getStack sv p = teStack $ svTrace sv !! p

getTmpExprTree :: SourceView c a u -> Int -> Forest Expr
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
microstep :: SourceView c a u -> Maybe (SourceView c a u)
microstep sv = 
    -- Try all transitions from the current location; choose the first successful one
    let transitions = Graph.lsuc (currentCFA sv) (currentLoc sv)
    in case mapMaybe (microstep' sv) transitions of
            []               -> trace ("microstep' returns false (currentLoc="  ++ (show $ currentLoc sv) ++ ")")
                                $ Nothing
            (store, stack):_ -> trace ("microstep': stack=" ++ show stack)
                                $ Just $ traceAppend sv store stack

microstep' :: SourceView c a u -> (Loc, TranLabel) -> Maybe (Store, EProcStack)
microstep' sv (to, TranCall meth retloc)  = -- insert new stack frame and mofify the old frame to point to return location,
                                            -- so that Return can be performed later
                                            let ?spec = svFlatSpec sv in
                                            let f0:frames = currentStackFrames sv
                                                stack' = f0{frLoc = retloc} : frames
                                                sc = F.ScopeMethod tmMain meth
                                             in if frScope f0 == sc -- avoid creating duplicate frames (happens with task calls)
                                                   then Just (currentStore sv, EProcStack $ f0{frLoc=to} : frames)
                                                   else Just (currentStore sv, EProcStack $ (FrameRegular sc to) : stack')
microstep' sv (_ , TranReturn)             = Just (currentStore sv, EProcStack $ tail $ currentStackFrames sv)
microstep' sv (to, TranNop)                = Just (currentStore sv, EProcStack $ (head $ currentStackFrames sv){frLoc = to} : (tail $ currentStackFrames sv))
microstep' sv (to, TranStat (SAssume e))   = if storeTryEvalBool (currentStore sv) e == Just True
                                                then Just (currentStore sv, EProcStack $ (head $ currentStackFrames sv){frLoc = to} : (tail $ currentStackFrames sv))
                                                else Nothing
microstep' sv (to, TranStat (SAssign l r)) = trace ("SAssign: " ++ show l ++ ":=" ++ show r) $
                                             let rval = storeTryEval (currentStore sv) r
                                                 store' = storeSet (currentStore sv) l rval
                                             in case rval of 
                                                     Nothing -> Nothing
                                                     _       -> Just (store', EProcStack $ (head $ currentStackFrames sv){frLoc = to} : (tail $ currentStackFrames sv))

makeTransition :: (D.Rel c v a s) => RSourceView c a u -> IO ()
makeTransition ref = do
    putStrLn "makeTransition"
    sv@SourceView{..} <- readIORef ref
    let store = applyExplicitUpdates sv
    model <- readIORef svModel
    let ?spec    = svSpec
        ?absvars = svAbsVars
        ?model   = model
        ?m       = D.mCtx model
    -- abstract final state
    mbstack <- currentMBStack sv
    let trans = D.abstractTransition svState store mbstack
        trans' = trans{D.tranSrc = Just $ show svPID}
    -- add transition
    D.modelAddTransition svModel trans'

applyExplicitUpdates :: SourceView c a u -> Store
applyExplicitUpdates sv = foldl' (\s (n, upd) -> let asn = snd $ fromJust $ find (storeEvalBool initstore . fst) upd
                                                 in storeSet s (EVar n) $ Just $ storeEval initstore asn) 
                                 (currentStore sv) (M.toList $ specUpds spec)
    where
    spec = svSpec sv
    -- store with state variables taken from the initial state of the transition and
    -- label variables -- from the final state
    initstore = storeUnion (initialStore sv) (storeProject (currentStore sv) (map varName $ specTmpVar spec))


setLPID :: PrID -> SourceView c a u -> SourceView c a u
setLPID pid sv = modifyCurrentStore sv (\s -> storeSet s mkPIDLVar (Just $ SVal $ EnumVal $ mkPIDEnumeratorName pid))

maybeSetLCont :: SourceView c a u -> SourceView c a u
maybeSetLCont sv | (isNothing $ storeTryEvalBool (currentStore sv) mkContLVar) = 
                   let cont = isProcControllableCode sv (svPID sv)
                   in modifyCurrentStore sv (\s -> storeSet s mkContLVar $ Just $ SVal $ BoolVal cont)
                 | otherwise                                                  = sv

-- Evaluate expression written in terms of variables in the original input spec.
storeEvalStr :: F.Spec -> F.Spec -> Spec -> Store -> Maybe PrID -> F.Scope -> String -> Either String Store
storeEvalStr inspec flatspec spec store mpid sc str = do
    -- Apply all transformations that the input spec goes through to the expression:
    -- 1. parse
    expr <- case parse (Grammar.detexprParser <* eof) "" str of
                 Left  e  -> Left $ show e
                 Right ex -> Right ex
    let (scope, iid) = flatScopeToScope inspec sc
    -- 2. validate
    let ?spec  = inspec
        ?privoverride = True 
        ?ispec = spec
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
                     , ctxVar     = []
                     , ctxLabels  = []}
        iexpr = evalState (F.exprToIExprDet simpexpr) ctx
    -- 6. evaluate
    return $ storeEval store iexpr

compileMB :: SourceView c a u -> F.Scope -> PrID -> String -> Either String CFA
compileMB SourceView{..} sc pid str = do
    -- Apply all transformations that the input spec goes through to the statement:
    -- 1. parse
    stat <- liftM (F.sSeq F.nopos Nothing)
            $ case parse (Grammar.statements1Parser <* eof) "" str of
                   Left  e  -> Left $ show e
                   Right st -> do assert (case head st of 
                                               F.SMagic _ _ -> False
                                               _            -> True)
                                         (F.pos $ head st) "Magic block body can not start with a magic block"
                                  return st
    let (scope,iid) = flatScopeToScope svInputSpec sc
    -- 2. validate
    let ?spec = svInputSpec
        ?ispec = svSpec
        ?privoverride = False
    F.validateStat scope stat
    validateControllableStat scope stat
    -- 3. flatten
    let flatstat = F.statFlatten iid scope stat
    -- 4. simplify
    let ?spec = svFlatSpec
    let (simpstat, (_, vars)) = let ?scope = sc
                                in runState (F.statSimplify flatstat) (0,[])
    assert (null vars) (F.pos stat) "Statement too complex"
    -- 5. inline
    let ctx = CFACtx { ctxEPID    = Just EPIDCont
                     , ctxStack   = [(sc, error "return from magic block", Nothing, (scopeLMap (Just pid) sc))]
                     , ctxCFA     = newCFA sc simpstat true
                     , ctxBrkLocs = []
                     , ctxGNMap   = globalNMap
                     , ctxLastVar = 0
                     , ctxVar     = []
                     , ctxLabels  = []}
        ctx' = let ?procs = [] 
                   ?nestedmb = True
               in execState (do aft <- F.procStatToCFA simpstat cfaInitLoc
                                ctxFinal aft) ctx
    assert (null $ ctxVar ctx') (F.pos stat) "Cannot perform non-deterministic controllable action"
    let cfa   = cfaMapExpr (ctxCFA ctx') $ F.exprExpandLabels svSpec
        cfar  = cfaPruneUnreachable cfa [cfaInitLoc]
    -- magic blocks inside cfar cannot be followed by any additional transitions
    mapM_ (\mbloc -> assert (all ((flip elem) $ cfaFinal cfar) $ map fst $ cfaLocTrans cfar mbloc) (pos $ locAct $ cfaLocLabel mbloc cfa) 
                            "No statements are allowed after nested magic block")
          $ filter (isMBLoc cfar) $ cfaDelayLocs cfar
    return {-$ cfaTraceFile cfar "action"-} cfar

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
                                       F.SVarDecl p _ _ -> err p "Variable declarations are not allowed inside controllable actions"
                                       F.SReturn  p _ _ -> err p "Return statements are not allowed inside controllable actions"
                                       F.SPar     p _ _ -> err p "Fork statements are not allowed inside controllable actions"
                                       F.SStop    p _   -> err p "Stop statements are not allowed inside controllable actions"
                                       _                -> return st) sc stat
    return ()

flatScopeToScope :: F.Spec -> F.Scope -> (F.Scope, F.IID)
flatScopeToScope inspec sc = 
    let ?spec = inspec in
    case sc of
         F.ScopeMethod   _ meth -> let (i, mname) = F.itreeParseName $ F.sname meth
                                       tm = F.itreeTemplate i
                                       meth' = fromJust $ find ((== mname) . F.sname) $ F.tmAllMethod tm
                                   in (F.ScopeMethod tm meth', i)
         F.ScopeProcess  _ proc -> let (i, pname) = F.itreeParseName $ F.sname proc
                                       tm = F.itreeTemplate i
                                       proc' = fromJust $ find ((== pname) . F.sname) $ F.tmAllProcess tm
                                   in (F.ScopeProcess tm proc', i)
         F.ScopeTemplate _      -> (F.ScopeTop, [])
