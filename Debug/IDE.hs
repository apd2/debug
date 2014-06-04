module Debug.IDE(RIDE,
           IDEAlign(..),
           IDEPanel(..),
           IDEPanels(..),
           panelsAppend,
           panelCBNull,
           ideNew,
           ideWidget,
           ideAddLeft,
           ideAddCenter,
           ideAddRight,
           ideAddBottom,
           ideRemove,
           framePanelNew,
           panedPanelsNew,
           tabbedPanelsNew) where

import qualified Graphics.UI.Gtk as G
import Data.Maybe
import Data.IORef
import Control.Monad
import System.IO

data IDEAlign = AlignLeft
              | AlignCenter
              | AlignRight
              | AlignBottom

-- IDE panel callbacks 
data PanelCB = PanelCB {
    cbDelete :: IO ()
}

panelCBNull :: PanelCB
panelCBNull = PanelCB {cbDelete = return ()}

-- Interface to a widget that contains an individual IDE panel
data IDEPanel = IDEPanel {
    panelSetCB     :: PanelCB -> IO (),
    panelGetWidget :: IO G.Widget,
    panelGetName   :: IO String
}

-- Interface to a widget that contains a list of IDE panels
data IDEPanels = IDEPanels {
    -- Insert after index (-1 to insert before the first element)
    panelsInsert         :: Int -> G.Widget -> String -> IO (),
    panelsDelete         :: Int -> IO (),
    panelsGetIndex       :: G.Widget -> IO (Maybe Int),
    panelsGetNumChildren :: IO Int,
    -- top-level widget
    panelsGetWidget      :: IO G.Widget,
    panelsSetCB          :: PanelCB -> IO ()
}

panelsDeleteWidget :: IDEPanels -> G.Widget -> IO ()
panelsDeleteWidget panels w = do
    midx <- (panelsGetIndex panels) w
    case midx of
         Nothing  -> return ()
         Just idx -> (panelsDelete panels) idx

panelsAppend :: IDEPanels -> G.Widget -> String -> IO ()
panelsAppend panels w n = do
    num <- panelsGetNumChildren panels
    (panelsInsert panels) (num - 1) w n

panelsPrepend :: IDEPanels -> G.Widget -> String -> IO ()
panelsPrepend panels w n = (panelsInsert panels) (-1) w n

---------------------------------------------------------------
-- Implementation of IDEPanel based on GTK frame
---------------------------------------------------------------

data FramePanel = FramePanel {
    fpFrame       :: G.Frame,
    fpName        :: String,
    fpPanelCB     :: PanelCB,
    fpDeleteCB    :: IO ()
}

type RFramePanel = IORef FramePanel

framePanelNew :: G.Widget -> String -> IO () -> IO IDEPanel
framePanelNew w name deletecb = do
    frame <- G.frameNew
    G.widgetShow frame
    G.frameSetLabel frame name
    G.containerAdd frame w
    ref <- newIORef $ FramePanel { fpFrame    = frame
                                 , fpName     = name
                                 , fpPanelCB  = panelCBNull
                                 , fpDeleteCB = deletecb}
    return $ IDEPanel { panelSetCB     = fpSetCB     ref
                      , panelGetWidget = fpGetWidget ref
                      , panelGetName   = fpGetName   ref}


fpSetCB :: RFramePanel -> PanelCB -> IO ()
fpSetCB ref cb = do
    fp <- readIORef ref
    writeIORef ref $ fp {fpPanelCB = cb}


fpGetWidget :: RFramePanel -> IO G.Widget
fpGetWidget ref = do
    fp <- readIORef ref
    return $ G.toWidget $ fpFrame fp

fpGetName :: RFramePanel -> IO String
fpGetName ref = do
    fp <- readIORef ref
    return $ fpName fp


---------------------------------------------------------------
-- Implementation of IDEPanels based on Paned widgets
---------------------------------------------------------------
data PanedPanels = PanedPanels {
    ppMkPaned     :: IO G.Paned,
    ppFrame       :: G.Frame,
    ppNumChildren :: Int,
    ppPanes       :: [G.Paned],
    ppCB          :: PanelCB
}

type RPanedPanels = IORef PanedPanels

panedPanelsNew :: IO G.Paned -> IO IDEPanels
panedPanelsNew f = do
    frame <- G.frameNew
    G.frameSetShadowType frame G.ShadowNone
    --G.widgetSetSizeRequest frame ideDefaultWidth ideDefaultHeight
    G.widgetShow frame
    ref <- newIORef $ PanedPanels { ppMkPaned     = f
                                  , ppFrame       = frame
                                  , ppNumChildren = 0
                                  , ppPanes       = []
                                  , ppCB          = panelCBNull}

    return $ IDEPanels { panelsInsert         = ppInsert         ref
                       , panelsDelete         = ppDelete         ref
                       , panelsGetIndex       = ppGetIndex       ref
                       , panelsGetNumChildren = ppGetNumChildren ref
                       , panelsGetWidget      = ppWidget         ref
                       , panelsSetCB          = ppSetCB          ref}


ppInsert :: RPanedPanels -> Int -> G.Widget -> String -> IO ()
ppInsert ref i w _ = do
    pp <- readIORef ref
    case (ppNumChildren pp, i) of
         (0, _)  -> do G.containerAdd (ppFrame pp) w
                       writeIORef ref $ pp {ppNumChildren = 1}
         (_, -1) -> do -- create new top-level pane
                       pane <- ppMkPaned pp
                       G.widgetShow pane
                       G.panedAdd1 pane w
                       oldw <- (liftM fromJust) $ G.binGetChild $ ppFrame pp
                       G.containerRemove (ppFrame pp) oldw
                       G.panedAdd2 pane oldw
                       G.containerAdd (ppFrame pp) pane
                       writeIORef ref $ pp {ppNumChildren = ppNumChildren pp + 1, ppPanes = pane:(ppPanes pp)}
         (1, _)  -> do pane <- ppMkPaned pp
                       G.widgetShow pane
                       G.panedAdd2 pane w
                       oldw <- (liftM fromJust) $ G.binGetChild $ ppFrame pp
                       G.containerRemove (ppFrame pp) oldw
                       G.panedAdd1 pane oldw
                       G.containerAdd (ppFrame pp) pane
                       writeIORef ref $ pp {ppNumChildren = ppNumChildren pp + 1, ppPanes = [pane]}
         (_,_)   -> if i == ppNumChildren pp - 1
                       then do pane <- ppMkPaned pp
                               G.widgetShow pane
                               G.panedAdd2 pane w
                               oldw <- (liftM fromJust) $ G.panedGetChild2 $ last $ ppPanes pp
                               G.containerRemove (last $ ppPanes pp) oldw
                               G.panedAdd1 pane oldw
                               G.panedAdd2 (last $ ppPanes pp) pane
                               writeIORef ref $ pp {ppNumChildren = ppNumChildren pp + 1, ppPanes = (ppPanes pp)++[pane]}
                       else do pane <- ppMkPaned pp
                               G.widgetShow pane
                               G.panedAdd1 pane w
                               oldw <- (liftM fromJust) $ G.panedGetChild2 $ ppPanes pp !! i
                               G.containerRemove (ppPanes pp !! i) oldw
                               G.panedAdd2 pane oldw
                               G.panedAdd2 (ppPanes pp !! i) pane
                               writeIORef ref $ pp {ppNumChildren = ppNumChildren pp + 1, ppPanes = (take i $ ppPanes pp)++[pane]++(drop i $ ppPanes pp)}


ppDelete :: RPanedPanels -> Int -> IO ()
ppDelete ref i = do
    pp <- readIORef ref
    case (ppNumChildren pp, i) of
         (1,_) -> do w <- (liftM fromJust) $ G.binGetChild $ ppFrame pp
                     G.containerRemove (ppFrame pp) w
                     writeIORef ref $ pp {ppNumChildren = 0}
         (_,0) -> do w1 <- (liftM fromJust) $ G.panedGetChild1 (head $ ppPanes pp)
                     w2 <- (liftM fromJust) $ G.panedGetChild2 (head $ ppPanes pp)
                     G.containerRemove (head $ ppPanes pp) w1
                     G.containerRemove (head $ ppPanes pp) w2
                     G.containerRemove (ppFrame pp) (head $ ppPanes pp)
                     G.containerAdd (ppFrame pp) w2
                     writeIORef ref $ pp {ppNumChildren = ppNumChildren pp - 1, ppPanes = tail $ ppPanes pp}
         (2,1) -> do w1 <- (liftM fromJust) $ G.panedGetChild1 (head $ ppPanes pp)
                     w2 <- (liftM fromJust) $ G.panedGetChild2 (head $ ppPanes pp)
                     G.containerRemove (head $ ppPanes pp) w1
                     G.containerRemove (head $ ppPanes pp) w2
                     G.containerRemove (ppFrame pp) (head $ ppPanes pp)
                     G.containerAdd (ppFrame pp) w1
                     writeIORef ref $ pp {ppNumChildren = 1, ppPanes = []}                     
         (_,_) -> if i == ppNumChildren pp - 1
                     then do let pane = last $ ppPanes pp
                                 prev = last $ init $ ppPanes pp
                             w1 <- (liftM fromJust) $ G.panedGetChild1 pane
                             w2 <- (liftM fromJust) $ G.panedGetChild2 pane
                             G.containerRemove pane w1
                             G.containerRemove pane w2
                             G.containerRemove prev pane
                             G.panedAdd2 prev w1
                             writeIORef ref $ pp {ppNumChildren = ppNumChildren pp - 1, ppPanes = init $ ppPanes pp}
                     else do let pane = ppPanes pp !! i
                                 prev = ppPanes pp !! (i-1)
                             w1 <- (liftM fromJust) $ G.panedGetChild1 pane
                             w2 <- (liftM fromJust) $ G.panedGetChild2 pane
                             G.containerRemove pane w1
                             G.containerRemove pane w2
                             G.containerRemove prev pane
                             G.panedAdd2 prev w2
                             writeIORef ref $ pp {ppNumChildren = ppNumChildren pp - 1, ppPanes = (take i $ ppPanes pp)++(drop (i+1) $ ppPanes pp)}
    if ppNumChildren pp == 1
       then cbDelete $ ppCB pp
       else return ()

ppGetIndex :: RPanedPanels -> G.Widget -> IO (Maybe Int)
ppGetIndex ref w = do
    pp <- readIORef ref
    if ppNumChildren pp == 1
       then do w0 <- (liftM fromJust) $ G.binGetChild $ ppFrame pp
               if w0 == w
                  then return $ Just 0
                  else return Nothing
       else ppGetIndex' (ppPanes pp) w 0

ppGetIndex' :: [G.Paned] -> G.Widget -> Int -> IO (Maybe Int)
ppGetIndex' []  _ _ = return Nothing
ppGetIndex' [p] w i = do
    w1 <- (liftM fromJust) $ G.panedGetChild1 p
    w2 <- (liftM fromJust) $ G.panedGetChild2 p
    if w1 == w
       then return $ Just i
       else if w2 == w
               then return $ Just $ i + 1
               else return Nothing
ppGetIndex' (p:ps) w i = do
    w1 <- (liftM fromJust) $ G.panedGetChild1 p
    if w1 == w
       then return $ Just i
       else ppGetIndex' ps w (i+1)

ppGetNumChildren :: RPanedPanels -> IO Int
ppGetNumChildren ref = do
    pp <- readIORef ref
    return $ ppNumChildren pp


ppWidget :: RPanedPanels -> IO G.Widget
ppWidget ref = do
    pp <- readIORef ref
    return $ G.toWidget $ ppFrame pp

ppSetCB :: RPanedPanels -> PanelCB -> IO ()
ppSetCB ref cb = do
    pp <- readIORef ref
    writeIORef ref $ pp {ppCB = cb}

---------------------------------------------------------------
-- Implementation of IDEPanels based on Notebook widget
---------------------------------------------------------------

data TabbedPanels = TabbedPanels {
    tpNotebook    :: G.Notebook,
    tpNumChildren :: Int,
    tpCB          :: PanelCB
}

type RTabbedPanels = IORef TabbedPanels

tabbedPanelsNew :: IO IDEPanels
tabbedPanelsNew = do
    nb <- G.notebookNew
    G.notebookSetScrollable nb True
    G.widgetShow nb
    ref <- newIORef $ TabbedPanels { tpNotebook    = nb
                                   , tpNumChildren = 0
                                   , tpCB          = panelCBNull}
    return $ IDEPanels { panelsInsert         = tpInsert         ref
                       , panelsDelete         = tpDelete         ref
                       , panelsGetIndex       = tpGetIndex       ref
                       , panelsGetNumChildren = tpGetNumChildren ref
                       , panelsGetWidget      = tpWidget         ref
                       , panelsSetCB          = tpSetCB          ref}

tpInsert :: RTabbedPanels -> Int -> G.Widget -> String ->  IO ()
tpInsert ref i w n = do
    tp <- readIORef ref
    let i' = if i < tpNumChildren tp - 1
                then i + 1
                else -1
    G.notebookInsertPage (tpNotebook tp) w n i'
    writeIORef ref $ tp {tpNumChildren = tpNumChildren tp + 1}

tpDelete :: RTabbedPanels -> Int -> IO ()
tpDelete ref i = do
    tp <- readIORef ref
    G.notebookRemovePage (tpNotebook tp) i
    writeIORef ref $ tp {tpNumChildren = tpNumChildren tp - 1}
    if tpNumChildren tp == 1
       then cbDelete $ tpCB tp
       else return ()
   
tpGetIndex :: RTabbedPanels -> G.Widget -> IO (Maybe Int)
tpGetIndex ref w = do
    tp <- readIORef ref
    G.notebookPageNum (tpNotebook tp) w

tpGetNumChildren :: RTabbedPanels -> IO Int
tpGetNumChildren ref = do
    tp <- readIORef ref
    return $ tpNumChildren tp

tpWidget :: RTabbedPanels -> IO G.Widget
tpWidget ref = do
    tp <- readIORef ref
    return $ G.toWidget $ tpNotebook tp

tpSetCB :: RTabbedPanels -> PanelCB -> IO ()
tpSetCB ref cb = do
    tp <- readIORef ref
    writeIORef ref $ tp {tpCB = cb}

---------------------------------------------------------
-- Debugger IDE
---------------------------------------------------------

data IDE = IDE {
    ideMain      :: IDEPanels,
    ideLeft      :: Maybe IDEPanels,
    ideTopBottom :: Maybe IDEPanels,
    ideTop       :: Maybe IDEPanels,
    ideBottom    :: Maybe IDEPanels,
    ideCenter    :: Maybe IDEPanels,
    ideRight     :: Maybe IDEPanels
}

type RIDE = IORef IDE

ideNew :: IO RIDE
ideNew = do
    main <- panedPanelsNew ((liftM G.toPaned) G.hPanedNew)
    newIORef $ IDE { ideMain      = main
                   , ideLeft      = Nothing
                   , ideTopBottom = Nothing
                   , ideTop       = Nothing
                   , ideBottom    = Nothing
                   , ideCenter    = Nothing
                   , ideRight     = Nothing}

ideWidget :: RIDE -> IO G.Widget
ideWidget ref = do
    ide <- readIORef ref
    panelsGetWidget $ ideMain ide

-- Create left panel if one does not exist
ideCreateLeft :: RIDE -> IO ()
ideCreateLeft ref = do
   ide <- readIORef ref
   case ideLeft ide of
        Just _  -> return ()
        Nothing -> do left <- panedPanelsNew ((liftM G.toPaned) G.vPanedNew)
                      w <- panelsGetWidget left
                      panelsPrepend (ideMain ide) w ""
                      panelsSetCB left (PanelCB $ ideDeleteLeft ref)
                      writeIORef ref $ ide {ideLeft = Just left}

ideDeleteLeft :: RIDE -> IO ()
ideDeleteLeft ref = do
    ide <- readIORef ref
    w <- panelsGetWidget $ fromJust $ ideLeft ide
    writeIORef ref $ ide {ideLeft = Nothing}
    panelsDeleteWidget (ideMain ide) w

ideCreateTopBottom :: RIDE -> IO ()
ideCreateTopBottom ref = do
    ide <- readIORef ref
    case ideTopBottom ide of
         Just _  -> return ()
         Nothing -> do tb <- panedPanelsNew ((liftM G.toPaned) G.vPanedNew)
                       w <- panelsGetWidget tb
                       panelsAppend (ideMain ide) w ""
                       panelsSetCB tb (PanelCB $ ideDeleteTopBottom ref) 
                       writeIORef ref $ ide {ideTopBottom = Just tb}

ideDeleteTopBottom :: RIDE -> IO ()
ideDeleteTopBottom ref = do
    ide <- readIORef ref
    w <- panelsGetWidget $ fromJust $ ideTopBottom ide
    writeIORef ref $ ide {ideTopBottom = Nothing}
    panelsDeleteWidget (ideMain ide) w


ideCreateBottom :: RIDE -> IO ()
ideCreateBottom ref = do
    ideCreateTopBottom ref
    ide <- readIORef ref
    case ideBottom ide of
         Just _  -> return ()
         Nothing -> do bottom <- tabbedPanelsNew
                       w <- panelsGetWidget bottom
                       panelsAppend (fromJust $ ideTopBottom ide) w ""
                       panelsSetCB bottom (PanelCB $ ideDeleteBottom ref)
                       writeIORef ref $ ide {ideBottom = Just bottom}

ideDeleteBottom :: RIDE -> IO ()
ideDeleteBottom ref = do
    ide <- readIORef ref
    w <- panelsGetWidget $ fromJust $ ideBottom ide
    writeIORef ref $ ide {ideBottom = Nothing}
    panelsDeleteWidget (fromJust $ ideTopBottom ide) w


ideCreateTop :: RIDE -> IO ()
ideCreateTop ref = do
    ideCreateTopBottom ref
    ide <- readIORef ref
    case ideTop ide of
         Just _  -> return ()
         Nothing -> do top <- panedPanelsNew ((liftM G.toPaned) G.hPanedNew)
                       w <- panelsGetWidget top
                       panelsPrepend (fromJust $ ideTopBottom ide) w ""
                       panelsSetCB top (PanelCB $ ideDeleteTop ref)
                       writeIORef ref $ ide {ideTop = Just top}

ideDeleteTop :: RIDE -> IO ()
ideDeleteTop ref = do
    ide <- readIORef ref
    w <- panelsGetWidget $ fromJust $ ideTop ide
    writeIORef ref $ ide {ideTop = Nothing}
    panelsDeleteWidget (fromJust $ ideTopBottom ide) w

ideCreateCenter :: RIDE -> IO ()
ideCreateCenter ref = do
    ideCreateTop ref
    ide <- readIORef ref
    case ideCenter ide of
         Just _  -> return ()
         Nothing -> do center <- panedPanelsNew ((liftM G.toPaned) G.hPanedNew)
                       w <- panelsGetWidget center
                       panelsPrepend (fromJust $ ideTop ide) w ""
                       panelsSetCB center (PanelCB $ ideDeleteCenter ref)
                       writeIORef ref $ ide {ideCenter = Just center}

ideDeleteCenter :: RIDE -> IO ()
ideDeleteCenter ref = do
    ide <- readIORef ref
    w <- panelsGetWidget $ fromJust $ ideCenter ide
    writeIORef ref $ ide {ideCenter = Nothing}
    panelsDeleteWidget (fromJust $ ideTop ide) w

ideCreateRight :: RIDE -> IO ()
ideCreateRight ref = do
    ideCreateTop ref
    ide <- readIORef ref
    case ideRight ide of
         Just _  -> return ()
         Nothing -> do right <- panedPanelsNew ((liftM G.toPaned) G.vPanedNew)
                       w <- panelsGetWidget right
                       panelsAppend (fromJust $ ideTop ide) w ""
                       panelsSetCB right (PanelCB $ ideDeleteRight ref)
                       writeIORef ref $ ide {ideRight = Just right}

ideDeleteRight :: RIDE -> IO ()
ideDeleteRight ref = do
    ide <- readIORef ref
    w <- panelsGetWidget $ fromJust $ ideRight ide
    writeIORef ref $ ide {ideRight = Nothing}
    panelsDeleteWidget (fromJust $ ideTop ide) w


ideAddLeft :: RIDE -> IDEPanel -> IO ()
ideAddLeft ref panel = do
    ideCreateLeft ref
    ide <- readIORef ref
    w <- panelGetWidget panel
    n <- panelGetName   panel
    panelsAppend (fromJust $ ideLeft ide) w n
    panelSetCB panel (PanelCB $ panelsDeleteWidget (fromJust $ ideLeft ide) w)

ideAddRight :: RIDE -> IDEPanel -> IO ()
ideAddRight ref panel = do
    ideCreateRight ref
    ide <- readIORef ref
    w <- panelGetWidget panel
    n <- panelGetName   panel
    panelsAppend (fromJust $ ideRight ide) w n
    panelSetCB panel (PanelCB $ panelsDeleteWidget (fromJust $ ideRight ide) w)

ideAddCenter :: RIDE -> IDEPanel -> IO ()
ideAddCenter ref panel = do
    ideCreateCenter ref
    ide <- readIORef ref
    w <- panelGetWidget panel
    n <- panelGetName   panel
    panelsAppend (fromJust $ ideCenter ide) w n
    panelSetCB panel (PanelCB $ panelsDeleteWidget (fromJust $ ideCenter ide) w)

ideAddBottom :: RIDE -> IDEPanel -> IO ()
ideAddBottom ref panel = do
    ideCreateBottom ref
    ide <- readIORef ref
    w <- panelGetWidget panel
    n <- panelGetName   panel
    panelsAppend (fromJust $ ideBottom ide) w n
    panelSetCB panel (PanelCB $ panelsDeleteWidget (fromJust $ ideBottom ide) w)

ideRemove :: RIDE -> IDEPanel -> IO ()
ideRemove ref panel = do
    ide <- readIORef ref
    w   <- panelGetWidget panel
    case ideLeft ide of
         Nothing -> return ()
         Just p  -> panelsDeleteWidget p w
    case ideCenter ide of
         Nothing -> return ()
         Just p  -> panelsDeleteWidget p w
    case ideRight ide of
         Nothing -> return ()
         Just p  -> panelsDeleteWidget p w
    case ideBottom ide of
         Nothing -> return ()
         Just p  -> panelsDeleteWidget p w
