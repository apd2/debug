{-# LANGUAGE ImplicitParams #-}

module DbgGUI(debugGUI) where

import qualified Graphics.UI.Gtk as G
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Data.List
import Data.Maybe
import System.Glib.MainLoop

import Util
import DbgTypes
import IDE
import Icon
import VarView
import GraphView
import Implicit

dbgDefaultWidth  = 1024
dbgDefaultHeight = 768

data Debugger a b = Debugger {
    dbgIDE      :: RIDE,
    dbgViews    :: [DbgView a b]
}

type RDebugger a b = IORef (Debugger a b)

-- State associated by the debugger with each registered view (visible or invisible)
data DbgView a b = DbgView {
    dbgViewView     :: View a b,
    dbgViewPanel    :: IDEPanel,
    dbgViewVisible  :: Bool,
    dbgViewMenuItem :: G.CheckMenuItem
}


dbgGetIDE :: RDebugger a b -> IO RIDE
dbgGetIDE ref = (liftM dbgIDE) $ readIORef ref

dbgGetView :: RDebugger a b -> Int -> IO (DbgView a b)
dbgGetView ref id = do
    dbg <- readIORef ref
    return $ dbgViews dbg !! id

dbgSetView :: RDebugger a b -> Int -> DbgView a b -> IO ()
dbgSetView ref id view = do
    dbg <- readIORef ref
    writeIORef ref $ dbg {dbgViews = (take id (dbgViews dbg)) ++ [view] ++ drop (id+1) (dbgViews dbg)}

-- List of available views
viewFactories :: (Rel c v a s, Vals b) => [(RModel c a b -> IO (View a b), Bool)]
viewFactories = [ (varViewNew,   True)
                , (graphViewNew, True)]


-- GUI manager
debugGUI :: (Rel c v a s, Vals b) => Model c a b -> IO ()
debugGUI model = do
    let ?m = mCtx model
    let factories = viewFactories
    rmodel <- newIORef model

    -- Initialize GTK+ engine
    G.initGUI 

    -- main window
    wmain <- G.windowNew
    G.widgetSetSizeRequest wmain dbgDefaultWidth dbgDefaultHeight
    G.windowMaximize wmain
    G.on wmain G.deleteEvent (do {liftIO G.mainQuit; return True})

    -- use the same icon for all debugger windows
    icon <- G.pixbufNewFromXPMData ladyBugIcon
    G.windowSetTitle wmain "Termite debugger"
    G.windowSetIcon wmain (Just icon)
    G.windowSetDefaultIcon (Just icon)

    -- vbox to hold window content
    vbox <- G.vBoxNew False 0
    G.containerAdd wmain vbox

    -- main menu
    mmain <- G.menuBarNew
    G.boxPackStart vbox mmain G.PackNatural 3

    -- view selection menu
    mview <- G.menuNew
    iview <- G.menuItemNewWithLabel "View"
    G.menuItemSetSubmenu iview mview
    G.menuShellAppend mmain iview

    -- IDE manager
    ide <- ideNew
    idew <- ideWidget ide

    -- Create debugger state
    ref <- newIORef $ Debugger { dbgIDE   = ide
                               , dbgViews = []}

    G.boxPackStart vbox idew G.PackGrow 0


    views <- mapM (\f -> (fst f) rmodel) factories
    modifyIORef rmodel (\m -> m{mViews = views})

    dviews <- mapIdxM (\v id -> do mitem <- G.checkMenuItemNewWithLabel (viewName v)
                                   G.menuShellAppend mview mitem
                                   G.on mitem G.checkMenuItemToggled (dbgViewToggle ref id mitem)
                                   G.widgetShow mitem                                 
                                   w <- viewGetWidget v
                                   panel <- framePanelNew w (viewName v) (dbgViewHideCB ref id)
                                   return $ DbgView { dbgViewView     = v
                                                    , dbgViewPanel    = panel
                                                    , dbgViewVisible  = False
                                                    , dbgViewMenuItem = mitem})
                      views

    dbg <- readIORef ref
    writeIORef ref $ dbg {dbgViews = dviews}

    G.widgetShowAll wmain

    mapM (\(v,s) -> G.checkMenuItemSetActive (dbgViewMenuItem v) s)
         $ zip dviews (map snd factories)

    let initst = fmap ((\rel -> State rel Nothing) . fromJust . oneCube (mStateV model) . snd) 
                      $ find ((== "init") . fst) $ mStateRels model
    modelSelectState rmodel initst

    G.mainGUI
    putStrLn "exiting debugger"


dbgViewToggle :: RDebugger a b -> Int -> G.CheckMenuItem -> IO ()
dbgViewToggle ref id item = do
    active <- G.checkMenuItemGetActive item
    view <- dbgGetView ref id
    if active && (not $ dbgViewVisible view)
       then dbgViewShow ref id
       else if (not active) && (dbgViewVisible view)
               then dbgViewHide ref id
               else return ()


dbgViewShow :: RDebugger a b -> Int  -> IO ()
dbgViewShow ref id = do
    view <- dbgGetView ref id
    ide  <- dbgGetIDE ref
    let align = viewDefAlign $ dbgViewView view
        panel = dbgViewPanel view
    case align of
         AlignLeft   -> ideAddLeft   ide panel
         AlignCenter -> ideAddCenter ide panel
         AlignRight  -> ideAddRight  ide panel
         AlignBottom -> ideAddBottom ide panel
    dbgSetView ref id $ view {dbgViewVisible = True}
    viewShow $ dbgViewView view


dbgViewHide :: RDebugger a b -> Int  -> IO ()
dbgViewHide ref id = do
    view <- dbgGetView ref id
    viewHide $ dbgViewView view
    ide  <- dbgGetIDE ref
    let panel = dbgViewPanel view
    ideRemove ide panel
    dbgSetView ref id $ view {dbgViewVisible = False}

-- Callback triggered by a view when it wants to hide itself.
-- Simply toggle the menu item to unchecked state.  Do the actual
-- deletion in the event handler.
dbgViewHideCB :: RDebugger a b -> Int -> IO ()
dbgViewHideCB ref id = do
    view <- dbgGetView ref id
    G.checkMenuItemSetActive (dbgViewMenuItem view) False
