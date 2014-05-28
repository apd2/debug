{-# LANGUAGE ImplicitParams #-}

module DbgGUI(debugGUI) where

import qualified Graphics.UI.Gtk as G
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef

import Util
import DbgTypes
import IDE
import Icon
import VarView
import GraphView
import RelationView

dbgDefaultWidth  = 640
dbgDefaultHeight = 480

data Debugger a b d = Debugger {
    dbgIDE      :: RIDE,
    dbgViews    :: [DbgView a b d]
}

type RDebugger a b d = IORef (Debugger a b d)

-- State associated by the debugger with each registered view (visible or invisible)
data DbgView a b d = DbgView {
    dbgViewView     :: View a b d,
    dbgViewPanel    :: IDEPanel,
    dbgViewVisible  :: Bool,
    dbgViewMenuItem :: G.CheckMenuItem
}


dbgGetIDE :: RDebugger a b d -> IO RIDE
dbgGetIDE ref = (liftM dbgIDE) $ readIORef ref

dbgGetView :: RDebugger a b d -> Int -> IO (DbgView a b d)
dbgGetView ref idx = do
    dbg <- readIORef ref
    return $ dbgViews dbg !! idx

dbgSetView :: RDebugger a b d -> Int -> DbgView a b d -> IO ()
dbgSetView ref idx view = do
    dbg <- readIORef ref
    writeIORef ref $ dbg {dbgViews = (take idx (dbgViews dbg)) ++ [view] ++ drop (idx+1) (dbgViews dbg)}

-- List of available views
viewFactories :: (Rel c v a s, Vals b, Vals d) => [(RModel c a b d -> IO (View a b d), Bool)]
viewFactories = [ (varViewNew     ,   True)
                , (relationViewNew,   True)
                , (graphViewNew   , True)]

-- GUI manager
debugGUI :: (Rel c v a s, Vals b, Vals d) => [(RModel c a b d -> IO (View a b d), Bool)] -> Model c a b d -> IO ()
debugGUI extraFactories model = do
    let ?m = mCtx model
    let factories = viewFactories ++ extraFactories
    rmodel <- newIORef $ mUpdateTRel model

    -- Initialize GTK+ engine
    _ <- G.initGUI 

    -- main window
    wmain <- G.windowNew
    G.windowSetDefaultSize wmain dbgDefaultWidth dbgDefaultHeight
    G.windowMaximize wmain
    _ <- G.on wmain G.deleteEvent $ do q <- liftIO $ modelQuit rmodel
                                       liftIO $ when q G.mainQuit
                                       return True

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

    views <- mapM (\f -> do view <- (fst f) rmodel
                            modifyIORef rmodel (\m -> m{mViews = mViews m ++ [view]})
                            return view)
         factories

    dviews <- mapIdxM (\v idx -> do mitem <- G.checkMenuItemNewWithLabel (viewName v)
                                    G.menuShellAppend mview mitem
                                    _ <- G.on mitem G.checkMenuItemToggled (dbgViewToggle ref idx mitem)
                                    G.widgetShow mitem                                 
                                    w <- viewGetWidget v
                                    panel <- framePanelNew w (viewName v) (dbgViewHideCB ref idx)
                                    return $ DbgView { dbgViewView     = v
                                                     , dbgViewPanel    = panel
                                                     , dbgViewVisible  = False
                                                     , dbgViewMenuItem = mitem})
                      views

    dbg <- readIORef ref
    writeIORef ref $ dbg {dbgViews = dviews}

    G.widgetShowAll wmain

    _ <- mapM (\(v,s) -> G.checkMenuItemSetActive (dbgViewMenuItem v) s)
         $ zip dviews (map snd factories)

    putStr $ "Variable map\n" ++ mDumpIndices model ++ "\n"
    let initst = Just $ State (mInitState model) Nothing 
    modelSelectState rmodel initst

    G.mainGUI
    putStrLn "exiting debugger"


dbgViewToggle :: RDebugger a b d -> Int -> G.CheckMenuItem -> IO ()
dbgViewToggle ref idx item = do
    active <- G.checkMenuItemGetActive item
    view <- dbgGetView ref idx
    if active && (not $ dbgViewVisible view)
       then dbgViewShow ref idx
       else if (not active) && (dbgViewVisible view)
               then dbgViewHide ref idx
               else return ()


dbgViewShow :: RDebugger a b d -> Int  -> IO ()
dbgViewShow ref idx = do
    view <- dbgGetView ref idx
    ide  <- dbgGetIDE ref
    let align = viewDefAlign $ dbgViewView view
        panel = dbgViewPanel view
    case align of
         AlignLeft   -> ideAddLeft   ide panel
         AlignCenter -> ideAddCenter ide panel
         AlignRight  -> ideAddRight  ide panel
         AlignBottom -> ideAddBottom ide panel
    dbgSetView ref idx $ view {dbgViewVisible = True}
    viewShow $ dbgViewView view


dbgViewHide :: RDebugger a b d -> Int  -> IO ()
dbgViewHide ref idx = do
    view <- dbgGetView ref idx
    viewHide $ dbgViewView view
    ide  <- dbgGetIDE ref
    let panel = dbgViewPanel view
    ideRemove ide panel
    dbgSetView ref idx $ view {dbgViewVisible = False}

-- Callback triggered by a view when it wants to hide itself.
-- Simply toggle the menu item to unchecked state.  Do the actual
-- deletion in the event handler.
dbgViewHideCB :: RDebugger a b d -> Int -> IO ()
dbgViewHideCB ref idx = do
    view <- dbgGetView ref idx
    G.checkMenuItemSetActive (dbgViewMenuItem view) False
