{-# LANGUAGE ImplicitParams, RecordWildCards #-}

module VarView(RVarView,
               varViewNew) where

import qualified Graphics.UI.Gtk as G
import Data.IORef

import qualified DbgTypes        as D
import qualified IDE             as D
import SetExplorer
import Implicit

data VarView c a b d = VarView {
    vvModel     :: D.RModel c a b d,
    vvSelection :: Maybe (Either (D.State a d) (D.Transition a b d)),
    vvExplorer  :: RSetExplorer c a
}

vvSelectionFrom :: VarView c a b d -> Maybe (D.State a d)
vvSelectionFrom vv = case vvSelection vv of
                          Nothing         -> Nothing
                          Just (Left s)   -> Just s
                          Just (Right tr) -> Just (D.tranFrom tr)

vvSelectionTo :: VarView c a b d -> Maybe (D.State a d)
vvSelectionTo vv = case vvSelection vv of
                          Just (Right tr) -> Just (D.tranTo tr)
                          _               -> Nothing

type RVarView c a b d = IORef (VarView c a b d)

--------------------------------------------------------------
-- View callbacks
--------------------------------------------------------------

varViewNew :: (D.Rel c v a s) => D.RModel c a b d -> IO (D.View a b d)
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
    G.widgetSetSizeRequest vbox 300 300

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
                          , D.evtTRelUpdated        = update                    ref
                          }
    return $ D.View { D.viewName      = "Variables"
                    , D.viewDefAlign  = D.AlignLeft
                    , D.viewShow      = return ()
                    , D.viewHide      = return ()
                    , D.viewGetWidget = return $ G.toWidget vbox
                    , D.viewCB        = cb
                    }

varViewStateSelected :: (D.Rel c v a s) => RVarView c a b d -> Maybe (D.State a d) -> IO ()
varViewStateSelected ref mstate = do
    modifyIORef ref $ \vv -> vv {vvSelection = fmap Left mstate}
    update ref
--    putStrLn $ "trel support: " ++ (show $ supportIndices trel)

varViewTransitionSelected :: (D.Rel c v a s) => RVarView c a b d -> D.Transition a b d -> IO ()
varViewTransitionSelected ref tran = do
    modifyIORef ref $ \vv -> vv {vvSelection = Just $ Right tran}
    update ref

update :: (D.Rel c v a s) => RVarView c a b d -> IO ()
update ref = do
    VarView{..}  <- readIORef ref
    model@D.Model{..} <- readIORef vvModel
    let ?m = mCtx
    trel <- D.modelActiveTransRel vvModel
    let rel = case vvSelection of
                   Nothing         -> trel
                   Just (Left st)  -> (D.sAbstract st) .& trel 
                   Just (Right tr) -> D.tranRel model tr
    setExplorerSetRelation vvExplorer rel

---------------------------------------------------------------------
-- Private functions
---------------------------------------------------------------------

executeTransition :: (D.Rel c v a s, ?m::c) => RVarView c a b d -> IO ()
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
        tranSrc           = Nothing
    D.modelAddTransition vvModel D.Transition{..}
