{-# LANGUAGE ImplicitParams, RecordWildCards #-}

module Debug.MultiSetExplorer(RMultiSetExplorer,
                        multiSetExplorerNew,
                        multiSetExplorerSetRelation,
                        multiSetExplorerReset,
                        multiSetExplorerGetVarAssignment,
                        multiSetExplorerGetWidget) where

import qualified Graphics.UI.Gtk as G
import Data.IORef
import Data.Tuple.Select
import Control.Monad

import Util
import qualified Debug.DbgTypes        as D
import Debug.SetExplorer
import Implicit


data MultiSetExplorer c a = MultiSetExplorer {
    mseCtx            :: c,
    mseCB             :: SetExplorerEvents,
    mseRel            :: a,
    mseVBox           :: G.VBox,
    mseSections       :: [Section],
    mseExplorers      :: [RSetExplorer c a]
}

type RMultiSetExplorer c a = IORef (MultiSetExplorer c a)

----------------------------------------------------------
-- External interface
----------------------------------------------------------


multiSetExplorerNew :: D.Rel c v a s => c -> [Section] -> SetExplorerEvents -> IO (RMultiSetExplorer c a)
multiSetExplorerNew ctx sections cb = do
    let ?m = ctx
    ref <- newIORef undefined
    
    -- vbox to hold section explorers
    vbox <- G.vBoxNew False 0
    G.widgetShow vbox

    explorers <- mapIdxM (\section idx -> setExplorerNew ctx [section] (SetExplorerEvents (asnChanged ref idx))) sections

    -- pack section widgets in the vbox
    widgets <- mapM setExplorerGetWidget explorers
    _ <- mapM (\w -> G.boxPackStart vbox w G.PackNatural 0) widgets

    writeIORef ref $ MultiSetExplorer { mseCtx       = ctx
                                      , mseCB        = cb
                                      , mseRel       = b
                                      , mseVBox      = vbox
                                      , mseSections  = sections
                                      , mseExplorers = explorers }
    return ref


multiSetExplorerSetRelation :: (D.Rel c v a s) => RMultiSetExplorer c a -> a -> IO ()
multiSetExplorerSetRelation ref rel = do 
    mse@MultiSetExplorer{..} <- readIORef ref
    let ?m = mseCtx
    let rel0 = projectSect mse rel 0
    setExplorerSetRelation (head mseExplorers) rel0

multiSetExplorerReset :: (D.Rel c v a s) => RMultiSetExplorer c a -> IO ()
multiSetExplorerReset ref = do
    MultiSetExplorer{..} <- readIORef ref
    _ <- mapM setExplorerReset mseExplorers
    return ()

multiSetExplorerGetVarAssignment :: (D.Rel c v a s) => RMultiSetExplorer c a -> IO [(String, [(String, a)])]
multiSetExplorerGetVarAssignment ref = do
    MultiSetExplorer{..} <- readIORef ref
    mapM (\((n,_,_),e) -> do asn <- (liftM concat) $ setExplorerGetVarAssignment e
                             return (n,asn)) 
         $ zip mseSections mseExplorers

multiSetExplorerGetWidget :: RMultiSetExplorer c a -> IO G.Widget
multiSetExplorerGetWidget ref = (liftM $ G.toWidget . mseVBox) $ readIORef ref

---------------------------------------------------------------------
-- Child callbacks
---------------------------------------------------------------------

-- One of the children has changed its variable assignment
asnChanged :: (D.Rel c v a s) => RMultiSetExplorer c a -> Int -> IO ()
asnChanged ref idx = do
    mse@MultiSetExplorer{..} <- readIORef ref
    let ?m = mseCtx
    constr <- (liftM conj) $ mapM childConstr $ take (idx+1) mseExplorers
    if idx < length mseExplorers - 1
       then setExplorerSetRelation (mseExplorers !! (idx + 1)) $ projectSect mse (mseRel .& constr) (idx + 1)
       else return ()

---------------------------------------------------------------------
-- Private functions
---------------------------------------------------------------------

-- project relation on one of the sections by quantifying away variables from 
-- other sections
projectSect :: (D.Rel c v a s, ?m::c) => MultiSetExplorer c a -> a -> Int -> a
projectSect MultiSetExplorer{..} rel sect = project ids rel
    where ids = concatMap D.mvarIdx $ sel3 $ mseSections !! sect

childConstr :: (D.Rel c v a s, ?m::c) => RSetExplorer c a -> IO a
childConstr ref = do
    asns <- setExplorerGetVarAssignment ref
    return $ conj $ map snd $ concat asns
