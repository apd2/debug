module DbgTypes(ViewFactory(..),
                Manager(..),
                View) where

import IDE

-- Internal debugger state
data DbgState = DbgState {
    -- transition relation

    -- symbol table
}

type RDbgState = IORef DbgState 

-- View interface
data View = View {
    viewName     :: String,
    viewDefAlign :: IDEAlign,
    viewShow     :: RDbgState -> IO G.Widget,
    viewHide     :: IO ()
}
