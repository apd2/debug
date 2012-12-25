{-# LANGUAGE MultiParamTypeClasses #-}

module DbgTypes(View(..),
                Type(..)) where

import qualified Graphics.UI.Gtk as G 
import Data.IORef
import Control.Monad

import IDE
import LogicClasses

class (Variable c v, 
       Shiftable c v a, 
       QBF c v a, 
       Eq a, 
       EqConst c v a, 
       Serialisable c a, 
       Satisfiable c v a s r, 
       BoolOp c v a, 
       EqRaw c v a r,
       CUDDLike c v a,
       Show a) => Rel c v a s r

data Type = Bool
          | SInt Int
          | UInt Int
          | Enum [String]

-- View interface
data View a = View {
    viewName      :: String,
    viewDefAlign  :: IDEAlign,
    viewShow      :: IO (),
    viewHide      :: IO (),
    viewGetWidget :: IO G.Widget,
    viewCB        :: ViewEvents a
}

data Transition a = Transition {
    tranRel :: a    
}

tranFromState :: Transition a -> a
tranFromState = undefined

tranToState   :: Transition a -> a
tranToState = undefined

tranLabel     :: Transition a -> a
tranLabel = undefined

data ViewEvents a = ViewEvents {
     evtStateSelected :: Maybe a -> IO (),
     evtTransition    :: Transition a -> IO ()
}

------------------------------------------------------
-- Top-level debugger state
------------------------------------------------------

data Model a = Model {
    mStateRels :: [(String, a)],
    mTransRels :: [(String, a)]
    --modelStateVars :: [(String, )]

}

type RModel a = IORef (Model a)

modelTransRels :: RModel a -> IO [(String, a)]
modelTransRels ref = (liftM mTransRels) $ readIORef ref

modelStateRels :: RModel a -> IO [(String, a)]
modelStateRels ref = (liftM mStateRels) $ readIORef ref
