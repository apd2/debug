module GraphView() where

import qualified Data.Graph.Inductive.Graph as G
import qualified DbgTypes                   as D

data StateCategory = StateControllable
                   | StateUncontrollable
                   | StateBoth

data State a = State {
    sState    :: a,
    sCategory :: StateCategory
}

data Edge a = EdgeTransition {eId :: Int, eLabel :: D.Label a}
            | EdgeSubset     {eId :: Int}
            | EdgeOverlap    {eId :: Int}

type TrGraph a = G.Gr (State a) (Edge a)

data GraphView a = GraphView {
    gvModel         :: D.RModel a,
    gvGraphDraw     :: RGraphDraw,
    gvGraph         :: TrGraph a,
    gvSelectedState :: Maybe G.Node,
    gvLastEdgeId    :: G.Edge
}

type RGraphDraw = IORef GraphView

--------------------------------------------------------------
-- View callbacks
--------------------------------------------------------------

graphViewNew :: D.RModel a -> IO D.View
graphViewNew model = do
    draw <- graphDrawNew
    ref <- newIORef $ GraphView { gvModel         = model
                                , gvGraphDraw     = graw
                                , gvGraphDraw     = G.empty
                                , gvSelectedState = Nothing
                                , gvLastEdgeId    = 0
                                }
    let cb = ViewEvents { evtStateSelected = graphViewStateSelected ref 
                        , evtTransition    = graphViewTransition    ref
                        }
    return $ View { viewName      = "Transition graph"
                  , viewDefAlign  = D.AlignCenter,
                  , viewShow      = return (),
                  , viewHide      = return (),
                  , viewGetWidget = graphDrawWidget draw,
                  , viewCB        = cb
                  }

graphViewStateSelected :: RGraphView a -> Maybe a -> IO ()
graphViewStateSelected ref mrel = do
    -- If selected state is equal to one of states in the graph, highlight this state
    gv <- readIORef ref
    let id = case mrel of
                  Nothing  -> Nothing
                  Just rel -> findState gv st
    gv' <- setSelected gv id
    writeIORef ref gv'

graphViewTransition :: RGraphView a -> a -> D.Label a -> a -> IO ()
graphViewTransition ref from lab to = do
    gv <- readIORef ref
    -- Find or create from-state
    (fromid, gv1) <- findOrCreateState gv (gvSelectedState gv) from
    -- Find or create to-state
    (toid, gv2)   <- findOrCreateState gv1 (Just fromid) to
    -- Add transition
    (_, gv3)      <- findOrCreateTransition gv2 fromid toid lab
    writeIORef ref gv3

--------------------------------------------------------------
-- Private functions
--------------------------------------------------------------

findState :: GraphView a -> a -> Maybe G.Node
findState gv rel = fmap fst $ find (\(id, s) -> sState s == rel) $ G.labNodes $ gvGraph gv

findTransition :: GraphView a -> G.Node -> G.Node -> D.Label a -> Maybe G.Edge
findTransition gv fromid toid lab = 
    fmap (\(_,_,e) -> eLabel e)
    $ find (\(f,t,e) -> f == fromid && t == toid &&
                        case e of 
                             EdgeTransition _ l -> l == lab
                             _                  -> False) 
    $ G.labEdges $ gvGraph gv

setSelected :: GraphView a -> (Maybe G.Node) -> IO (GraphView a)
setSelected gv mid = do
    let gv' = gv {gvSelectedState = mid}
    -- Deselect previous active node
    case gvSelectedState gv of
         Nothing -> return ()
         Just id -> graphDrawSetNodeStyle (gvGraphDraw gv) id (nodeStyle gv' id)
    -- Set new selection
    case mid of
         Nothing -> return ()
         Just id -> graphDrawSetNodeStyle (gvGraphDraw gv) id (nodeStyle gv' id)
    return gv'

findOrCreateState :: GraphView a -> Maybe G.Node -> a -> IO (G.Node, GraphView a)
findOrCreateState gv mid rel = 
    case findState gv rel of
         Just id -> return (id, gv)
         Nothing -> do coords <- chooseLocation gv mid
                       createState gv coords rel

findOrCreateTransition :: GraphView a -> G.Node -> G.Node -> D.Label a -> IO (G.Edge, GraphView a)
findOrCreateTransition :: gv fromid toid lab = do
    case findTransition gv fromid toid lab of
         Just id -> return (id, gv)
         Nothing -> createTransition gv fromid toid lab

createState :: GraphView a -> (Double, Double) -> a -> IO (G.Node, GraphView a)
createState gv coords rel = do
    cat <- D.stateCategory (gvModel gv) rel
    let id = (snd $ nodeRange (gvGraph gv)) + 1
        st = State rel cat
        gv' = G.insNode (id, st)
        (ls, as) = stateStyle gv' st
        annots = stateAnnots gv' st
        (subsets, other1)   = partition ((== t) . (.-> rel) . sState . getState gv) (G.nodes $ gvGraph gv)
        (supersets, other2) = partition ((== t) . (rel .->) . sState . getState gv) other1
        overlaps            = filter    ((/= b) . (.& rel)  . sState . getState gv) other2
    graphDrawInsertNode (gvGraphDraw gv') id annots coords ls as
    gv1 <- foldM (\gv sid -> (liftM snd) $ createSubsetEdge  gv sid id) gv' subsets
    gv2 <- foldM (\gv sid -> (liftM snd) $ createSubsetEdge  gv id sid) gv1 supersets
    gv3 <- foldM (\gv sid -> (liftM snd) $ createOverlapEdge gv id sid) gv2 overlaps
    return (id, gv3)

createTransition :: GraphView a -> G.Node -> G.Node -> D.Label a -> IO (G.Edge, GraphView a)
createTransition gv fromid toid lab = 
    createEdge gv fromid toid (\id -> EdgeTransition id lab) (transitionAnnots gv lab) transitionStyle EndArrow True

createSubsetEdge :: GraphView a -> G.Node -> G.Node -> IO (G.Edge, GraphView a)
createSubsetEdge gv fromid toid = 
    createEdge gv fromid toid EdgeSubset [] subsetStyle EndDiamond True

createOverlapEdge :: GraphView a -> G.Node -> G.Node -> IO (G.Edge, GraphView a)
createOverlapEdge gv fromid toid = 
    createEdge gv fromid toid EdgeOverlap [] overlapStyle EndNone True

createEdge :: GraphView a -> G.Node -> G.Node -> (G.Edge -> Edge) -> [GAnnotation] -> GC -> EndStyle -> Bool -> IO (GraphView a)
createEdge gv fromid toid f annots gc end visible = do
    let id = gvLastEdgeId gv + 1
        e = f id
        gv' = gv { gvGraph      = insEdge (fromid, toid, e) (gvGraph gv)
                 , gvLastEdgeId = id}
    graphDrawInsertEdge (gvGraphDraw gv') fromid toid id annots gc end visible
    return (id, gv')


chooseLocation :: GraphView a -> Maybe G.Node -> IO (Double, Double)

stateStyle :: GraphView a -> G.Node -> (GC, GC)

stateAnnots :: GraphView a -> G.Node -> [GAnnotation]

transitionAnnots :: GraphView a -> Label a -> [GAnnotation]
