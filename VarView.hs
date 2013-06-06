{-# LANGUAGE ImplicitParams, RecordWildCards #-}

module VarView(RVarView,
               varViewNew) where

import qualified Graphics.UI.Gtk as G
import Data.IORef

import qualified DbgTypes        as D
import qualified IDE             as D
import SetExplorer
import Implicit

data VarView c a b = VarView {
    vvModel     :: D.RModel c a b,
    vvSelection :: Maybe (Either (D.State a b) (D.Transition a b)),
    vvExplorer  :: RSetExplorer c a
}

vvSelectionFrom :: VarView c a b -> Maybe (D.State a b)
vvSelectionFrom vv = case vvSelection vv of
                          Nothing         -> Nothing
                          Just (Left s)   -> Just s
                          Just (Right tr) -> Just (D.tranFrom tr)

vvSelectionTo :: VarView c a b -> Maybe (D.State a b)
vvSelectionTo vv = case vvSelection vv of
                          Just (Right tr) -> Just (D.tranTo tr)
                          _               -> Nothing

type RVarView c a b = IORef (VarView c a b)

--------------------------------------------------------------
-- View callbacks
--------------------------------------------------------------

varViewNew :: (D.Rel c v a s) => D.RModel c a b -> IO (D.View a b)
varViewNew rmodel = do
    model <- readIORef rmodel
    let ?m = D.mCtx model
    -- Set explorer for choosing transition variables
    let sections = [ ("State Variables",      True,  D.mCurStateVars   model)
                   , ("Untracked Variables",  False, D.mUntrackedVars  model)
                   , ("Label Variables",      False, D.mLabelVars      model)
                   , ("Next-state Variables", True,  D.mNextStateVars  model)
                   ]
    explorer         <- setExplorerNew ?m sections (SetExplorerEvents {evtValueChanged = return ()})
    w                <- setExplorerGetWidget explorer
    ref <- newIORef $ VarView { vvModel     = rmodel
                              , vvSelection = Nothing 
                              , vvExplorer  = explorer
                              }
    -- Top-level vbox
    vbox <- G.vBoxNew False 0
    G.widgetShow vbox

    G.boxPackStart vbox w G.PackGrow 0
    
    -- Control buttons
    bbox <- G.hButtonBoxNew
    G.widgetShow bbox
    G.boxPackStart vbox bbox G.PackNatural 0

    resetbutton <- G.buttonNewFromStock G.stockClear
    _ <- G.on resetbutton G.buttonActivated (setExplorerReset explorer)
    G.widgetShow resetbutton
    G.boxPackStart bbox resetbutton G.PackNatural 10

    runbutton <- G.buttonNewFromStock G.stockApply
    G.widgetShow runbutton
    _ <- G.on runbutton G.buttonActivated (executeTransition ref)
    G.boxPackStart bbox runbutton G.PackNatural 0

    let cb = D.ViewEvents { D.evtStateSelected      = varViewStateSelected      ref 
                          , D.evtTransitionSelected = varViewTransitionSelected ref
                          }
    return $ D.View { D.viewName      = "Variables"
                    , D.viewDefAlign  = D.AlignLeft
                    , D.viewShow      = return ()
                    , D.viewHide      = return ()
                    , D.viewGetWidget = return $ G.toWidget vbox
                    , D.viewCB        = cb
                    }

varViewStateSelected :: (D.Rel c v a s) => RVarView c a b -> Maybe (D.State a b) -> IO ()
varViewStateSelected ref mstate = do
    vv@VarView{..} <- readIORef ref
    ctx <- D.modelCtx vvModel
    let ?m = ctx
    trel <- D.modelActiveTransRel vvModel
    writeIORef ref $ vv {vvSelection = fmap Left mstate}
--    putStrLn $ "trel support: " ++ (show $ supportIndices trel)
    setExplorerSetRelation vvExplorer $ case mstate of
                                             Nothing    -> trel
                                             Just state -> (D.sAbstract state) .& trel

varViewTransitionSelected :: (D.Rel c v a s) => RVarView c a b -> D.Transition a b -> IO ()
varViewTransitionSelected ref tran = do
    vv@VarView{..} <- readIORef ref
    model <- readIORef vvModel
    let ?m = D.mCtx model
    writeIORef ref $ vv {vvSelection = Just $ Right tran}
    setExplorerSetRelation vvExplorer (D.tranRel model tran)

---------------------------------------------------------------------
-- Private functions
---------------------------------------------------------------------

executeTransition :: (D.Rel c v a s, ?m::c) => RVarView c a b -> IO ()
executeTransition ref = do
    vv@VarView{..} <- readIORef ref
    [from, untracked, label, to] <- setExplorerGetVarAssignment vvExplorer 
    model <- readIORef vvModel
    let fabs              = conj $ map snd $ from
        tranFrom          = D.State { sAbstract = fabs
                                    , sConcrete = case vvSelectionFrom vv of
                                                       Nothing -> Nothing
                                                       Just st -> if D.sAbstract st .== fabs then D.sConcrete st else Nothing}
        tranUntracked     = conj $ map snd $ untracked 
        tranAbstractLabel = conj $ map snd $ label
        tranConcreteLabel = Nothing
        tabs              = swap (D.mNextV model) (D.mStateV model) (conj $ map snd $ to)
        tranTo            = D.State { sAbstract = tabs
                                    , sConcrete = case vvSelectionTo vv of
                                                       Nothing -> Nothing
                                                       Just st -> if D.sAbstract st  .== tabs then D.sConcrete st else Nothing}
    D.modelSelectTransition vvModel D.Transition{..}
