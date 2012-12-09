module DbgTypes(ViewFactory(..),
                Manager(..),
                View) where

-- Internal debugger state
data DbgState = DbgState {
    -- transition relation

    -- symbol table
}


data Manager = Manager {
--    createWindow :: 
}


-- View interface
data ViewFactory = ViewFactory {
    viewName   :: String,
    viewCreate :: Manager -> IO View
}

data View = View {
    viewDestroy :: IO ()

    -- callbacks
}
