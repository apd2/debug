module DbgTypes(View(..),
                DbgView(..),
                Debugger(..),
                RDebugger,
                dbgGetIDE,
                dbgGetView,
                dbgSetView) where

import qualified Graphics.UI.Gtk as G 
import Data.IORef

import IDE

-- View interface
data View = View {
    viewName      :: String,
    viewDefAlign  :: IDEAlign,
    viewShow      :: RDebugger -> IO (),
    viewHide      :: RDebugger -> IO (),
    viewGetWidget :: IO G.Widget
}

------------------------------------------------------
-- Top-level debugger state
------------------------------------------------------

-- State associated by the debugger with each registered view (visible or invisible)
data DbgView = DbgView {
    dbgViewView     :: View,
    dbgViewPanel    :: IDEPanel,
    dbgViewVisible  :: Bool,
    dbgViewMenuItem :: G.CheckMenuItem
}

data Debugger = Debugger {
    dbgIDE      :: RIDE,
    dbgViews    :: [DbgView]
}

type RDebugger = IORef Debugger

dbgGetIDE :: RDebugger -> IO RIDE
dbgGetIDE ref = do
    dbg <- readIORef ref
    return $ dbgIDE dbg

dbgGetView :: RDebugger -> Int -> IO DbgView
dbgGetView ref id = do
    dbg <- readIORef ref
    return $ dbgViews dbg !! id

dbgSetView :: RDebugger -> Int -> DbgView -> IO ()
dbgSetView ref id view = do
    dbg <- readIORef ref
    writeIORef ref $ dbg {dbgViews = (take id (dbgViews dbg)) ++ [view] ++ drop (id+1) (dbgViews dbg)}

