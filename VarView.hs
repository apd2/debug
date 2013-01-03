{-# LANGUAGE ImplicitParams, RecordWildCards #-}

module VarView(RVarView,
               varViewNew) where

import qualified Graphics.UI.Gtk as G
import Data.IORef
import Control.Monad

import Util
import qualified DbgTypes        as D
import qualified IDE             as D
--import MultiSetExplorer
import SetExplorer
import Implicit

data VarView c a = VarView {
    vvModel    :: D.RModel c a,
    vvExplorer :: RSetExplorer c a
}

type RVarView c a = IORef (VarView c a)

--------------------------------------------------------------
-- View callbacks
--------------------------------------------------------------

varViewNew :: (D.Rel c v a s) => D.RModel c a -> IO (D.View a)
varViewNew model = do
    ctx              <- D.modelCtx model
    let ?m = ctx
    -- Set explorer for choosing transition variables
    stateSection     <- (liftM $ map (mapTrd3 fst)) $ D.modelStateVars model
    untrackedSection <- D.modelUntrackedVars model
    labelSection     <- D.modelLabelVars     model
    nextSection      <- (liftM $ map (mapTrd3 snd)) $ D.modelStateVars model
    let sections = [ ("State Variables",      True,  stateSection)
                   , ("Untracked Variables",  False, untrackedSection)
                   , ("Label Variables",      False, labelSection)
                   , ("Next-state Variables", True,  nextSection)
                   ]
    explorer         <- setExplorerNew ctx sections (SetExplorerEvents {evtValueChanged = return ()})
    w                <- setExplorerGetWidget explorer
    ref <- newIORef $ VarView { vvModel    = model 
                              , vvExplorer = explorer
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
    G.on resetbutton G.buttonActivated (setExplorerReset explorer)
    G.widgetShow resetbutton
    G.boxPackStart bbox resetbutton G.PackNatural 10

    runbutton <- G.buttonNewFromStock G.stockApply
    G.widgetShow runbutton
    G.on runbutton G.buttonActivated (executeTransition ref)
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

varViewStateSelected :: (D.Rel c v a s) => RVarView c a -> Maybe a -> IO ()
varViewStateSelected ref mrel = do
    VarView{..} <- readIORef ref
    ctx <- D.modelCtx vvModel
    let ?m = ctx
    trel <- D.modelActiveTransRel vvModel
    putStrLn $ "trel support: " ++ (show $ supportIndices trel)
    setExplorerSetRelation vvExplorer $ case mrel of
                                             Nothing  -> trel
                                             Just rel -> rel .& trel

varViewTransitionSelected :: (D.Rel c v a s) => RVarView c a -> D.Transition a -> IO ()
varViewTransitionSelected ref tran = do
    vv@VarView{..} <- readIORef ref
    model <- readIORef vvModel
    let ?m = D.mCtx model
    setExplorerSetRelation vvExplorer (conj [D.tranFrom tran, D.tranUntracked tran, D.tranLabel tran, D.tranTo' tran])

---------------------------------------------------------------------
-- Private functions
---------------------------------------------------------------------

executeTransition :: (D.Rel c v a s, ?m::c) => RVarView c a -> IO ()
executeTransition ref = do
    VarView{..} <- readIORef ref
    [from, untracked, label, to] <- setExplorerGetVarAssignment vvExplorer 
    D.modelSelectTransition vvModel D.Transition { D.tranFrom      = conj $ map snd $ from
                                                 , D.tranUntracked = conj $ map snd $ untracked 
                                                 , D.tranLabel     = conj $ map snd $ label
                                                 , D.tranTo'       = conj $ map snd $ to}
