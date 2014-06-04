{-# LANGUAGE RecordWildCards, ImplicitParams #-}

module Debug.RelationView (relationViewNew) where

import Data.IORef
import Control.Monad
import qualified Graphics.UI.Gtk            as G

import Util
import qualified Debug.DbgTypes                   as D
import qualified Debug.IDE                        as D
import Implicit

--------------------------------------------------------------
-- Types
--------------------------------------------------------------

data RelationView c a b d = RelationView {
    rvModel :: D.RModel c a b d,
    rvTrans :: [(String, a, G.CheckButton)]    -- transition relations
    }


type RRelationView c a b d = IORef (RelationView c a b d)

--------------------------------------------------------------
-- View callbacks
--------------------------------------------------------------
relationViewNew :: (D.Rel c v a s) => D.RModel c a b d -> IO (D.View a b d)
relationViewNew model = do
    ref <- newIORef $ RelationView { rvModel    = model
                                   , rvTrans    = error "relationView: rvTrans undefined" }
    trels  <- D.modelTransRels model
    scroll <- G.scrolledWindowNew Nothing Nothing
    G.widgetShow scroll
    G.widgetSetSizeRequest scroll 300 300

    vbox <- G.vBoxNew False 0
    G.widgetShow vbox

    G.scrolledWindowAddWithViewport scroll vbox

    trels' <- mapM (\(n,_,r) -> do enbut <- G.checkButtonNewWithLabel n
                                   _ <- G.on enbut G.toggled (update ref)
                                   G.widgetShow enbut
                                   G.boxPackStart vbox enbut G.PackNatural 0
                                   return (n,r,enbut))
              trels

    modifyIORef ref $ \rv -> rv {rvTrans = trels'}

    _ <- mapM (\((_,e,_), (_,_,enbut)) -> G.toggleButtonSetActive enbut e) 
         $ zip trels trels'

    return $ D.View { D.viewName      = "Relations"
                    , D.viewDefAlign  = D.AlignLeft
                    , D.viewShow      = return ()
                    , D.viewHide      = return ()
                    , D.viewGetWidget = return $ G.toWidget scroll
                    , D.viewQuit      = return True
                    , D.viewCB        = D.ViewEvents { D.evtStateSelected      = (\_ -> return ())
                                                     , D.evtTransitionSelected = (\_ -> return ())
                                                     , D.evtTRelUpdated        = return ()
                                                     }
                    }

--------------------------------------------------------------
-- GUI Actions
--------------------------------------------------------------

update :: (D.Rel c v a s) => RRelationView c a b d -> IO ()
update ref = do
    RelationView{..} <- readIORef ref
    m <- D.modelCtx rvModel
    let ?m = m
    trel <- liftM conj $ mapM (\(_,r,but) -> do en <- G.toggleButtonGetActive but
                                                if' en (return r) (return t)) rvTrans
    D.modelSetConstraint rvModel "Transition relation" (Just trel)
    return ()
