module Manager(debugGUI) where

import qualified Graphics.UI.Gtk as G
import Control.Concurrent

import DbgTypes
import Icon


views :: [ViewFactory]
views = [graphView, variableView]

graphView :: ViewFactory
graphView = ViewFactory {viewName   = "Transition graph view",
                         viewCreate = graphViewCreate}

graphViewCreate :: Manager -> IO View
graphViewCreate m = undefined

variableView :: ViewFactory
variableView = ViewFactory {viewName   = "Variables view",
                            viewCreate = graphViewCreate}

variableViewCreate :: Manager -> IO View
variableViewCreate m = undefined

data ManagerState = ManagerState {
                        msLeft,
                        msRight,
                        msBottom,

                        msLeftPane,  
                        msRightPane,
                        msTopPane,
                        msBottomPane,
                        msCentrePane,
                        msRightPane
                    }

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

    -- create menu item for each available view
    mapM (\v -> do mitem <- checkMenuItemNewWithLabel (viewName v)
                   menuShellAppend mview mitem
                   widgetShow mitem) 
         views

    widgetShowAll wmain
    mainGUI
