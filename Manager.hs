module Manager(debugGUI) where

import qualified Graphics.UI.Gtk as G
import Control.Concurrent

import DbgTypes
import Icon


views :: [View]
views = [graphView, variableView]

graphView :: View
graphView = View { viewName     = "Transition graph view"
                 , viewDefAlign = AlignCenter
                 , viewShow     = graphViewShow
                 , viewHide     = return ()}

graphViewShow :: Manager -> IO View
graphViewShow m = undefined

variableView :: View
variableView = View { viewName     = "Variables view"
                    , viewDefAlign = AlignLeft
                    , viewShow     = variableViewShow
                    , viewHide     = return ()}

variableViewShow :: Manager -> IO View
variableViewShow m = undefined

-- GUI manager
debugGUI :: IO ()
debugGUI = do
    -- Initialize GTK+ engine
    initGUI 
    -- Every so often, we try to run other threads.
    timeoutAddFull (yield >> return True) priorityDefaultIdle 100

    -- main window
    wmain <- windowNew

    -- use the same icon for all debugger windows
    icon <- pixbufNewFromXPMData ladyBugIcon
    windowSetTitle wmain "Termite debugger"
    windowSetIcon wmain (Just icon)
    windowSetDefaultIcon (Just icon)

    -- vbox to hold window content
    vbox <- vBoxNew False 0
    containerAdd wmain vbox

    -- main menu
    mmain <- menuBarNew
    boxPackStart vbox mmain PackNatural 3

    -- view selection menu
    mview <- menuNew
    iview <- menuItemNewWithLabel "View"
    menuItemSetSubmenu iview mview
    menuShellAppend mmain iview

    -- IDE manager
    ide <- ideNew
    idew <- ideWidget ide

    boxPackStart vbox idew PackNatural 0

    -- create menu item for each available view
    mapM (\v -> do mitem <- checkMenuItemNewWithLabel (viewName v)
                   menuShellAppend mview mitem
                   widgetShow mitem) 
         views

    widgetShowAll wmain
    mainGUI
