module IDE(IDE) where

import qualified Graphics.UI.Gtk as G

data IDEPanels {
    -- Insert after index (-1 to insert before the first element)
    panelsInsert :: Int -> G.Widget -> IO (),

    panelsDelete :: Int -> IO (),

    -- top-level widget
    panelsWidget :: IO G.Widget
}

-- Implementation of IDEPanels based on Paned widgets
data PanedPanels = PanedPanels {
    ppMkPaned     :: IO Paned,
    ppBox         :: G.VBox,
    ppNumChildren :: Int,
    ppPanes       :: [G.Paned]
}


panedPanelsNew :: IO Paned -> IO IDEPanels
panedPanelsNew f = do
    box <- vBoxNew
    ref <- newIORef $ PanedPanels f box 0 []
    return $ IDEPanels { panelsInsert = ppInsert ref
                       , panelsDelete = ppDelete ref
                       , panelsWidget = ppWidget ref}


ppInsert :: IORef PanedPanels -> Int -> G.Widget -> IO ()
ppInsert ref i w = do
    pp <- readIORef ref
    case (ppNumChildren pp, i) of
         (0, _)  -> do G.boxPackStart (ppBox pp) w G.PackGrow 0
                       writeIORef ref $ pp {ppNumChildren = 1}
         (_, -1) -> do -- create new top-level pane
                       pane <- ppMkPaned pp
                       G.widgetShow pane
                       G.panedAdd1 pane w
                       oldw <- (liftM head) $ G.containerGetChildren $ ppBox pp
                       G.containerRemove (ppBox pp) oldw
                       G.panedAdd2 pane oldw
                       G.boxPackStart (ppBox pp) pane
                       writeIORef ref $ pp {ppNumChildren = ppNumChildren pp + 1, ppPanes = [pane]}
         (1, _)  -> do pane <- ppMkPaned pp
                       G.widgetShow pane
                       G.panedAdd2 pane w
                       oldw <- (liftM head) $ G.containerGetChildren $ ppBox pp
                       G.containerRemove (ppBox pp) oldw
                       G.panedAdd1 pane oldw
                       G.boxPackStart (ppBox pp) pane
                       writeIORef ref $ pp {ppNumChildren = ppNumChildren pp + 1, ppPanes = [pane]}
         (_,_)   -> if i == ppNumChildren pp - 1
                       then do pane <- ppMkPaned pp
                               G.widgetShow pane
                               G.panedAdd2 pane w
                               oldw <- G.panedGetChild2 $ tail $ ppPanes pp
                               G.containerRemove (tail $ ppPanes pp) oldw
                               G.panedAdd1 pane oldw
                               G.panedAdd2 (tail $ ppPanes pp) pane
                               writeIORef ref $ pp {ppNumChildren = ppNumChildren pp + 1, ppPanes = (ppPanes pp)++[pane]}
                       else do pane <- ppMkPaned pp
                               G.widgetShow pane
                               G.panedAdd1 pane w
                               oldw <- G.panedGetChild2 $ ppPanes pp !! i
                               G.containerRemove (ppPanes pp !! i) oldw
                               G.panedAdd2 pane oldw
                               G.panedAdd2 (ppPanes pp !! i) pane
                               writeIORef ref $ pp {ppNumChildren = ppNumChildren pp + 1, ppPanes = (take i pp)++[pane]++(drop i pp)}
         


ppDelete :: IORef PanedPanels -> Int -> IO ()
ppDelete ref i = do
    pp <- readIORef ref
    case (ppNumChildren pp, i) of
         (1,_) -> do w <- (liftM head) $ containerGetChildren $ ppBox pp
                     G.containerRemove (ppBox pp) w
                     writeIORef ref $ pp {ppNumChildren = 0}
         (_,0) -> do w1 <- G.panedGetChild1 (head $ ppPanes p)
                     w2 <- G.panedGetChild2 (head $ ppPanes p)
                     G.containerRemove (head $ ppPanes p) w1
                     G.containerRemove (head $ ppPanes p) w2
                     G.containerRemove (ppBox pp) (head $ ppPanes p)
                     G.boxPackStart (ppBox pp) w2
                     writeIORef ref $ pp {ppNumChildren = ppNumChildren pp - 1, ppPanes = tail $ ppPanes pp}
         (2,1) -> do w1 <- G.panedGetChild1 (head $ ppPanes p)
                     w2 <- G.panedGetChild2 (head $ ppPanes p)
                     G.containerRemove (head $ ppPanes p) w1
                     G.containerRemove (head $ ppPanes p) w2
                     G.containerRemove (ppBox pp) (head $ ppPanes p)
                     G.boxPackStart (ppBox pp) w1
                     writeIORef ref $ pp {ppNumChildren = 1, ppPanes = []}                     
         (_,_) -> if i == ppNumChildren pp - 1
                     then do let pane = last $ ppPanes pp
                                 prev = ppPanes pp !! (ppNumChildren pp - 3)
                             w1 <- G.panedGetChild1 pane
                             w2 <- G.panedGetChild2 pane
                             G.containerRemove pane w1
                             G.containerRemove pane w2
                             G.containerRemove prev pane
                             G.panedAdd2 prev w1
                             writeIORef ref $ pp {ppNumChildren = ppNumChildren pp - 1, ppPanes = init $ ppPanes pp}
                     else do let pane = ppPanes pp !! i
                                 prev = ppPanes pp !! (i-1)
                             w1 <- G.panedGetChild1 pane
                             w2 <- G.panedGetChild2 pane
                             G.containerRemove pane w1
                             G.containerRemove pane w2
                             G.containerRemove prev pane
                             G.paneAdd2 prev w2
                             writeIORef ref $ pp {ppNumChildren = ppNumChildren pp - 1, ppPanes = (take (i-1) $ ppPanes pp)++(drop i $ ppPanes p)}

ppWidget :: IORef PanedPanels -> IO G.Widget
ppWidget ref = do
    pp <- readIORef
    G.toWidget $ ppBox pp

data TabbedPanels = TabbedPanels {
}


