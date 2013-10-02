{-# LANGUAGE RecordWildCards, ImplicitParams #-}

module StrategyView (Strategy(..),
                     strategyViewNew) where

import Data.Maybe
import Data.List
import Data.IORef
import Control.Monad
import qualified Graphics.UI.Gtk            as G

import Util
import qualified DbgTypes                   as D
import qualified IDE                        as D
import Implicit

--------------------------------------------------------------
-- Types
--------------------------------------------------------------

-- Strategies and counterexamples
data Strategy a = Strategy {
    stratName  :: String,
    stratGoals :: [(String, a)],
    stratFair  :: [(String, a)],
    stratRel   :: [[a]]
}

data StrategyView c a b d = StrategyView {
    svModel          :: D.RModel c a b d,
    svStrat          :: Strategy a,
    svEnBut          :: G.CheckButton,
    svGoalButs       :: [G.RadioButton],
    svFairButs       :: [G.RadioButton]
    }


type RStrategyView c a b d = IORef (StrategyView c a b d)

--------------------------------------------------------------
-- View callbacks
--------------------------------------------------------------
strategyViewNew :: (D.Rel c v a s) => Strategy a -> D.RModel c a b d -> IO (D.View a b d)
strategyViewNew strat@Strategy{..} model = do
    ref <- newIORef $ StrategyView { svModel    = model
                                   , svStrat    = strat
                                   , svEnBut    = error "strategyView: svEnBut undefined"
                                   , svGoalButs = error "strategyView: svGoalButs undefined"
                                   , svFairButs = error "strategyView: svFairButs undefined"}

    scroll <- G.scrolledWindowNew Nothing Nothing
    G.widgetShow scroll
    G.widgetSetSizeRequest scroll 300 300

    vbox <- G.vBoxNew False 0
    G.widgetShow vbox

    G.scrolledWindowAddWithViewport scroll vbox

    -- Apply strategy check box
    apply <- G.checkButtonNewWithLabel "Use strategy"
    _ <- G.on apply G.toggled (update ref)
    G.widgetShow apply
    G.boxPackStart vbox apply G.PackNatural 0

    sep <- G.hSeparatorNew
    G.widgetShow sep
    G.boxPackStart vbox sep G.PackNatural 0

    hbox <- G.hBoxNew True 0
    G.widgetShow hbox
    G.boxPackStart vbox hbox G.PackGrow 0
    
    frm1 <- G.frameNew
    G.widgetShow frm1
    G.frameSetLabel frm1 "Goals"
    G.boxPackStart hbox frm1 G.PackGrow 0
    vbox1 <- G.vBoxNew False 0
    G.widgetShow vbox1
    G.containerAdd frm1 vbox1

    frm2 <- G.frameNew
    G.widgetShow frm2
    G.frameSetLabel frm2 "Fair regions"
    G.boxPackStart hbox frm2 G.PackGrow 0
    vbox2 <- G.vBoxNew False 0
    G.widgetShow vbox2
    G.containerAdd frm2 vbox2

    -- Goals
    gbuts <- mapIdxM (\(n,_) i -> do rad <- G.radioButtonNew
                                     _ <- G.on rad G.toggled (goalToggled ref i)
                                     G.widgetShow rad
                                     lab <- G.labelNew $ Just n
                                     G.widgetShow lab
                                     G.containerAdd rad lab
                                     G.boxPackStart vbox1 rad G.PackNatural 0
                                     return rad)
                     stratGoals


    -- Fair regions
    fbuts <- mapIdxM (\(n,_) i -> do rad <- G.radioButtonNew
                                     _ <- G.on rad G.toggled (fairToggled ref i)
                                     G.widgetShow rad
                                     lab <- G.labelNew $ Just n
                                     G.widgetShow lab
                                     G.containerAdd rad lab
                                     G.boxPackStart vbox2 rad G.PackNatural 0
                                     return rad)
                     stratFair

    modifyIORef ref $ \sv -> sv { svEnBut    = apply
                                , svGoalButs = gbuts
                                , svFairButs = fbuts}

    _ <- mapM (\bt -> G.radioButtonSetGroup bt $ head gbuts) $ drop 1 gbuts
    _ <- mapM (\bt -> G.radioButtonSetGroup bt $ head fbuts) $ drop 1 fbuts

    return $ D.View { D.viewName      = stratName
                    , D.viewDefAlign  = D.AlignLeft
                    , D.viewShow      = return ()
                    , D.viewHide      = G.toggleButtonSetActive apply False
                    , D.viewGetWidget = return $ G.toWidget scroll
                    , D.viewQuit      = return True
                    , D.viewCB        = D.ViewEvents { D.evtStateSelected      = (\mst -> do {highlightActive ref mst; maybe (return ()) (autoSchedule ref) mst})
                                                     , D.evtTransitionSelected = (\_   -> highlightActive ref Nothing)
                                                     , D.evtTRelUpdated        = return ()
                                                     }
                    }

--------------------------------------------------------------
-- GUI Actions
--------------------------------------------------------------

highlightActive :: (D.Rel c v a s) => RStrategyView c a b d -> Maybe (D.State a d) -> IO ()
highlightActive ref mst = do
    let (Just st) = mst
    StrategyView{..} <- readIORef ref
    let Strategy{..} = svStrat
    ctx <- D.modelCtx svModel
    let ?m = ctx
    _ <- mapIdxM (\bt i -> do lab <- (liftM $ G.castToLabel . fromJust) $ G.binGetChild bt
                              let w = if' ((isJust mst) && ((D.sAbstract st .& (snd $ stratGoals !! i)) .== b)) "bold" "normal"
                              G.labelSetMarkup lab $ "<span weight=\"" ++ w ++ "\">" ++ (fst $ stratGoals !! i) ++ "</span>")
                 svGoalButs
    _ <- mapIdxM (\bt i -> do lab <- (liftM $ G.castToLabel . fromJust) $ G.binGetChild bt
                              let w = if' ((isJust mst) && ((D.sAbstract st .& (snd $ stratFair !! i)) ./= b)) "bold" "normal"
                              G.labelSetMarkup lab $ "<span weight=\"" ++ w ++ "\">" ++ (fst $ stratFair !! i) ++ "</span>") 
                 svFairButs
    return ()

goalToggled :: (D.Rel c v a s) => RStrategyView c a b d -> Int -> IO ()
goalToggled ref i = do
    buts <- getIORef svGoalButs ref
    on <- G.toggleButtonGetActive $ buts !! i
    -- Every toggle triggers two event notifications (for the disabled and 
    -- enabled button). Only react to the enabling event
    when on $ update ref

fairToggled :: (D.Rel c v a s) => RStrategyView c a b d -> Int -> IO ()
fairToggled ref i = do
    buts <- getIORef svFairButs ref
    on <- G.toggleButtonGetActive $ buts !! i
    when on $ update ref

update :: (D.Rel c v a s) => RStrategyView c a b d -> IO ()
update ref = do
    StrategyView{..} <- readIORef ref
    let Strategy{..} = svStrat
    en  <- G.toggleButtonGetActive svEnBut
    gen <- (liftM $ findIndex (==True)) $ mapM G.toggleButtonGetActive svGoalButs
    fen <- (liftM $ findIndex (==True)) $ mapM G.toggleButtonGetActive svFairButs
    let constr = if' (not en)                         Nothing
               $ if' (isNothing gen || isNothing fen) Nothing
               $ Just $ stratRel !! fromJust gen !! fromJust fen
    D.modelSetConstraint svModel stratName constr

--------------------------------------------------------------
-- Automatic scheduling of goals and fair regions
--------------------------------------------------------------

autoSchedule :: (D.Rel c v a s) => RStrategyView c a b d -> D.State a d -> IO ()
autoSchedule ref st = do
    StrategyView{..} <- readIORef ref
    let Strategy{..} = svStrat
    ctx <- D.modelCtx svModel
    let ?m = ctx
    gen <- (liftM $ findIndex (==True)) $ mapM G.toggleButtonGetActive svGoalButs
    fen <- (liftM $ findIndex (==True)) $ mapM G.toggleButtonGetActive svFairButs
  
    -- find all non-empty substrategies for the selected state 
    let avl = map (map (\r -> (r .& D.sAbstract st) ./= b)) stratRel

    -- If the current fair region does not have strategy for the current state,
    -- select a different fair region.
    when (isJust gen && isJust fen) $
         when (not $ avl !! fromJust gen !! fromJust fen) $
              maybe (return ()) (\i -> G.toggleButtonSetActive (svFairButs !! i) True)
              $ findIndex (==True) (avl !! fromJust gen)
