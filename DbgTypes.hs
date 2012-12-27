{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}

module DbgTypes(Rel,
                View(..),
                ViewEvents(..),
                Type(..),
                Transition(..),
                RModel,
                modelCtx,
                modelStateVars,
                modelLabelVars,
                modelUntrackedVars,
                modelStateRels,
                modelTransRels,
                modelActiveTransRel,
                modelSelectTransition,
                modelSelectState) where

import qualified Graphics.UI.Gtk as G 
import Data.IORef
import Control.Monad

import IDE
import LogicClasses

------------------------------------------------------
-- Types
------------------------------------------------------

class (Variable c v, 
       VarDecl c v,
       Shiftable c v a, 
       QBF c v a, 
       Eq a, 
       EqConst c v a, 
       Serialisable c a, 
       Satisfiable c v a s [Bool], 
       BoolOp c v a, 
       EqRaw c v a [Bool],
       CUDDLike c v a,
       Cubeable c v a,
       Show a) => Rel c v a s

-- Debugger's own type system
data Type = Bool
          | SInt Int
          | UInt Int
          | Enum [String]

instance Show Type where
    show Bool      = "bool"
    show (SInt i)  = "sint<" ++ show i ++ ">"
    show (UInt i)  = "uint<" ++ show i ++ ">"
    show (Enum es) = "enum"

-- View interface
data View a = View {
    viewName      :: String,
    viewDefAlign  :: IDEAlign,
    viewShow      :: IO (),
    viewHide      :: IO (),
    viewGetWidget :: IO G.Widget,
    viewCB        :: ViewEvents a
}

-- Events sent from the debugger core to each view
data ViewEvents a = ViewEvents {
     evtStateSelected      :: Maybe a -> IO (),
     evtTransitionSelected :: Transition a -> IO ()
}

data Transition a = Transition {
    tranFrom      :: a,
    tranUntracked :: a,
    tranLabel     :: a,
    tranTo        :: a
}

-- Debugger state
data Model c a = Model {
    mCtx           :: c,
    mStateVars     :: [(String, Type, ([Int],[Int]))],
    mUntrackedVars :: [(String, Type, [Int])],
    mLabelVars     :: [(String, Type, [Int])],

    mStateRels     :: [(String, a)],
    mTransRels     :: [(String, a)],

    mViews         :: [View a]
}

type RModel c a = IORef (Model c a)


----------------------------------------------------------
-- External interface
----------------------------------------------------------

-- Querying state
modelCtx :: RModel c a -> IO c
modelCtx ref = (liftM mCtx) $ readIORef ref

modelStateVars :: RModel c a -> IO [(String, Type, ([Int],[Int]))]
modelStateVars ref = (liftM mStateVars) $ readIORef ref

modelUntrackedVars :: RModel c a -> IO [(String, Type, [Int])]
modelUntrackedVars ref = (liftM mUntrackedVars) $ readIORef ref

modelLabelVars :: RModel c a -> IO [(String, Type, [Int])]
modelLabelVars ref = (liftM mLabelVars) $ readIORef ref

modelTransRels :: RModel c a -> IO [(String, a)]
modelTransRels ref = (liftM mTransRels) $ readIORef ref

modelStateRels :: RModel c a -> IO [(String, a)]
modelStateRels ref = (liftM mStateRels) $ readIORef ref

-- TODO: implement proper selection of transition relation to debug
modelActiveTransRel :: RModel c a -> IO a
modelActiveTransRel ref = (liftM (snd . head . mTransRels)) $ readIORef ref

-- Actions
modelSelectTransition :: RModel c a -> Transition a -> IO ()
modelSelectTransition ref tran = do
   views <- modelViews ref
   mapM (\v -> (evtTransitionSelected $ viewCB v) tran) views
   return ()

modelSelectState :: RModel c a -> Maybe a -> IO ()
modelSelectState ref mrel = do
   views <- modelViews ref
   mapM (\v -> (evtStateSelected $ viewCB v) mrel) views
   return ()

----------------------------------------------------------
-- Private functions
----------------------------------------------------------

modelViews :: RModel c a -> IO [View a]
modelViews ref = (liftM mViews) $ readIORef ref
