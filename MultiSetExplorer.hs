{-# LANGUAGE ImplicitParams, RecordWildCards #-}

module MultiSetExplorer(RMultiSetExplorer,
                        multiSetExplorerNew,
                        multiSetExplorerSetRelation,
                        multiSetExplorerReset,
                        multiSetExplorerGetVarAssignment,
                        multiSetExplorerGetWidget) where

import qualified Data.Set        as S
import qualified Graphics.UI.Gtk as G
import Data.IORef
import Control.Monad

import Util
import qualified DbgTypes        as D
import SetExplorer
import Implicit


type Section = (String, [(String, D.Type, [Int])])

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

    explorers <- mapIdxM (\(_, vars) idx -> setExplorerNew ctx vars (SetExplorerEvents (asnChanged ref idx))) sections

    -- pack section widgets in the vbox
    widgets <- mapM setExplorerGetWidget explorers
    mapM (\((n,_),w) -> do frame <- G.frameNew
                           G.frameSetLabel frame n
                           G.widgetShow frame
                           G.containerAdd frame w
                           G.boxPackStart vbox frame G.PackNatural 0)
         $ zip sections widgets

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
    let rel0 = project mse rel 0
    setExplorerSetRelation (head mseExplorers) rel0

multiSetExplorerReset :: (D.Rel c v a s) => RMultiSetExplorer c a -> IO ()
multiSetExplorerReset ref = do
    MultiSetExplorer{..} <- readIORef ref
    mapM setExplorerReset mseExplorers
    return ()

multiSetExplorerGetVarAssignment :: (D.Rel c v a s) => RMultiSetExplorer c a -> IO [(String, [(String, a)])]
multiSetExplorerGetVarAssignment ref = do
    MultiSetExplorer{..} <- readIORef ref
    mapM (\((n,_),e) -> do asn <- setExplorerGetVarAssignment e
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
       then setExplorerSetRelation (mseExplorers !! (idx + 1)) $ project mse (mseRel .& constr) (idx + 1)
       else return ()

---------------------------------------------------------------------
-- Private functions
---------------------------------------------------------------------

-- project relation on one of the sections by quantifying away variables from 
-- other sections
project :: (D.Rel c v a s, ?m::c) => MultiSetExplorer c a -> a -> Int -> a
project MultiSetExplorer{..} rel sect = exists (vconcat vs) rel
    where vs = map varAtIndex $ concatMap trd3 $ concatMap snd $ take sect mseSections ++ drop (sect+1) mseSections

childConstr :: (D.Rel c v a s, ?m::c) => RSetExplorer c a -> IO a
childConstr ref = do
    asns <- setExplorerGetVarAssignment ref
    return $ conj $ map snd asns
