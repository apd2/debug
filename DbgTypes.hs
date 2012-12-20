module DbgTypes(View(..)) where

import qualified Graphics.UI.Gtk as G 
import Data.IORef

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

-- View interface
data View a = View {
    viewName      :: String,
    viewDefAlign  :: IDEAlign,
    viewShow      :: IO (),
    viewHide      :: IO (),
    viewGetWidget :: IO G.Widget,
    viewCB        :: ViewEvents a
}

data ViewEvents a = ViewEvents {
     evtStateSelected :: Maybe a -> IO (),
     evtTransition    :: a -> Label a -> a -> IO ()
}

------------------------------------------------------
-- Top-level debugger state
------------------------------------------------------

data Model = Model

type RModel = IORef Model
