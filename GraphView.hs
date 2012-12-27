{-# LANGUAGE ScopedTypeVariables #-}

module GraphView(graphViewNew) where

import Data.IORef
import Data.List
import Data.Maybe

import Implicit
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.Tree  as G
import qualified DbgTypes                   as D
import qualified IDE                        as D
import GraphDraw

tranAnnotStyle      =   GC {gcFG = (65535, 0, 0),         gcLW=0, gcLS=True}
stateAnnotStyle     =   GC {gcFG = (65535, 0, 0),         gcLW=0, gcLS=True}
labelStyle          =   GC {gcFG = (0, 0, 0),             gcLW=0, gcLS=True}

transitionStyle     =   GC {gcFG = (0, 40000, 0),         gcLW=2, gcLS=True}
subsetStyle         =   GC {gcFG = (40000, 40000, 40000), gcLW=1, gcLS=True}
overlapStyle        =   GC {gcFG = (40000, 40000, 40000), gcLW=2, gcLS=False}

controllableStyle   = ( GC {gcFG = (0    ,     0, 40000), gcLW=2, gcLS=True}
                      , GC {gcFG = (0    , 65535, 0),     gcLW=0, gcLS=False})
uncontrollableStyle = ( GC {gcFG = (0    ,     0, 40000), gcLW=2, gcLS=True}
                      , GC {gcFG = (65535,     0, 0),     gcLW=0, gcLS=False})
bothStyle           = ( GC {gcFG = (0    ,     0, 40000), gcLW=2, gcLS=True}
                      , GC {gcFG = (40000, 40000, 40000), gcLW=0, gcLS=False})


data StateCategory = StateControllable
                   | StateUncontrollable
                   | StateBoth

data State a = State {
    sState    :: a,
    sCategory :: StateCategory
}

data Edge a = EdgeTransition {eId :: Int, eLabel :: a}
            | EdgeSubset     {eId :: Int}
            | EdgeOverlap    {eId :: Int}

type TrGraph a = G.Gr (State a) (Edge a)

data GraphView c a = GraphView {
    gvModel         :: D.RModel c a,
    gvGraphDraw     :: RGraphDraw,
    gvGraph         :: TrGraph a,
    gvSelectedState :: Maybe G.Node,
    gvLastEdgeId    :: G.Edge
}

type RGraphView c a = IORef (GraphView c a)

--------------------------------------------------------------
-- View callbacks
--------------------------------------------------------------

graphViewNew :: D.RModel c a -> IO D.View
graphViewNew model = do
    draw <- graphDrawNew
    ref <- newIORef $ GraphView { gvModel         = model
                                , gvGraphDraw     = draw
                                , gvGraph         = G.empty
                                , gvSelectedState = Nothing
                                , gvLastEdgeId    = 0
                                }
    let cb = D.ViewEvents { D.evtStateSelected      = graphViewStateSelected      ref 
                          , D.evtTransitionSelected = graphViewTransitionSelected ref
                          }
    return $ D.View { D.viewName      = "Transition graph"
                    , D.viewDefAlign  = D.AlignCenter
                    , D.viewShow      = return ()
                    , D.viewHide      = return ()
                    , D.viewGetWidget = graphDrawWidget draw
                    , D.viewCB        = cb
                    }

graphViewStateSelected :: RGraphView c a -> Maybe a -> IO ()
graphViewStateSelected ref mrel = do
    -- If selected state is equal to one of states in the graph, highlight this state
    gv <- readIORef ref
    let id = case mrel of
                  Nothing  -> Nothing
                  Just rel -> findState gv rel
    gv' <- setSelected gv id
    writeIORef ref gv'

graphViewTransitionSelected :: RGraphView c a -> D.Transition a -> IO ()
graphViewTransitionSelected ref tran = do
    gv <- readIORef ref
    -- Find or create from-state
    (fromid, gv1) <- findOrCreateState gv (gvSelectedState gv) $ D.tranFrom tran
    -- Find or create to-state
    (toid, gv2)   <- findOrCreateState gv1 (Just fromid) $ D.tranTo tran
    -- Add transition
    (_, gv3)      <- findOrCreateTransition gv2 fromid toid tran
    writeIORef ref gv3

--------------------------------------------------------------
-- Private functions
--------------------------------------------------------------

findState :: GraphView c a -> a -> Maybe G.Node
findState gv rel = fmap fst $ find (\(id, s) -> sState s == rel) $ G.labNodes $ gvGraph gv

getState :: GraphView  c a -> G.Node -> State a
getState gv id = fromJust $ G.lab (gvGraph gv) id

findTransition :: GraphView c a -> G.Node -> G.Node -> a -> Maybe G.Edge
findTransition gv fromid toid labrel = 
    fmap (\(_,_,e) -> eLabel e)
    $ find (\(f,t,e) -> f == fromid && t == toid &&
                        case e of 
                             EdgeTransition _ l -> l == labrel
                             _                  -> False) 
    $ G.labEdges $ gvGraph gv

setSelected :: GraphView c a -> (Maybe G.Node) -> IO (GraphView a)
setSelected gv mid = do
    let gv' = gv {gvSelectedState = mid}
    -- Deselect previous active node
    case gvSelectedState gv of
         Nothing -> return ()
         Just id -> graphDrawSetNodeStyle (gvGraphDraw gv) id (stateStyle gv' id)
    -- Set new selection
    case mid of
         Nothing -> return ()
         Just id -> graphDrawSetNodeStyle (gvGraphDraw gv) id (stateStyle gv' id)
    return gv'

findOrCreateState :: GraphView c a -> Maybe G.Node -> a -> IO (G.Node, GraphView a)
findOrCreateState gv mid rel = 
    case findState gv rel of
         Just id -> return (id, gv)
         Nothing -> do coords <- chooseLocation gv mid
                       createState gv coords rel

findOrCreateTransition :: GraphView c a -> G.Node -> G.Node -> a -> IO (G.Edge, GraphView a)
findOrCreateTransition gv fromid toid tranrel = do
    case findTransition gv fromid toid (D.tranLabel tranrel) of
         Just id -> return (id, gv)
         Nothing -> createTransition gv fromid toid tranrel

createState :: GraphView c a -> (Double, Double) -> a -> IO (G.Node, GraphView a)
createState gv coords rel = do
    cat <- D.stateCategory (gvModel gv) rel
    let id = (snd $ G.nodeRange (gvGraph gv)) + 1
        st = State {sState = rel, sCategory = cat}
        gv' = G.insNode (id, st)
        (ls, as) = stateStyle gv' st
        (subsets, other1)   = partition ((== t) . (.-> rel) . sState . getstate gv) (G.nodes $ gvGraph gv)
        (supersets, other2) = partition ((== t) . (rel .->) . sState . getState gv) other1
        overlaps            = filter    ((/= b) . (.& rel)  . sState . getState gv) other2
    annots <- stateAnnots gv' rel
    graphDrawInsertNode (gvGraphDraw gv') id annots coords ls as
    gv1 <- foldM (\gv sid -> (liftM snd) $ createSubsetEdge  gv sid id) gv' subsets
    gv2 <- foldM (\gv sid -> (liftM snd) $ createSubsetEdge  gv id sid) gv1 supersets
    gv3 <- foldM (\gv sid -> (liftM snd) $ createOverlapEdge gv id sid) gv2 overlaps
    return (id, gv3)

createTransition :: GraphView c a -> G.Node -> G.Node -> a -> IO (G.Edge, GraphView c a)
createTransition gv fromid toid tranrel = do
    annots <- transitionAnnots gv tranrel
    createEdge gv fromid toid (\id -> EdgeTransition id (D.tranLabel tranrel)) annots transitionStyle EndArrow True

createSubsetEdge :: GraphView c a -> G.Node -> G.Node -> IO (G.Edge, GraphView a)
createSubsetEdge gv fromid toid = 
    createEdge gv fromid toid EdgeSubset [] subsetStyle EndDiamond True

createOverlapEdge :: GraphView c a -> G.Node -> G.Node -> IO (G.Edge, GraphView a)
createOverlapEdge gv fromid toid = 
    createEdge gv fromid toid EdgeOverlap [] overlapStyle EndNone True

createEdge :: GraphView c a -> G.Node -> G.Node -> (G.Edge -> Edge) -> [GAnnotation] -> GC -> EndStyle -> Bool -> IO (GraphView a)
createEdge gv fromid toid f annots gc end visible = do
    let id = gvLastEdgeId gv + 1
        e = f id
        gv' = gv { gvGraph      = insEdge (fromid, toid, e) (gvGraph gv)
                 , gvLastEdgeId = id}
    graphDrawInsertEdge (gvGraphDraw gv') fromid toid id annots gc end visible
    return (id, gv')


chooseLocation :: GraphView c a -> Maybe G.Node -> IO (Double, Double)
chooseLocation gv Nothing = chooseLocationFrom gv initLocation
chooseLocation gv (Just id) = do
    (parentx, parenty) <- graphDrawGetNodeCoords (gvGraphDraw gv) id
    chooseLocationFrom gv (parentx, parenty + childOffset)

-- Find unoccupied location next to given coordinates
chooseLocationFrom :: GraphView c a -> (Double, Double) -> IO (Double, Double)
chooseLocationFrom gv (x,y) = do
    mid <- graphDrawGetNodeAtLocation (x,y)
    case mid of
         Nothing -> return (x,y)
         Just _  -> chooseLocationFrom gv (x,y+graphSearchStep)

stateStyle :: GraphView c a -> G.Node -> (GC, GC)
stateStyle gv id = 
    if Just id == gvSelectedState gv
       then (ls{gcLW = gcLW ls + 1}, as{gcFG = map3 (/2,/2,/2) $ gcFG as})
       else (ls,as)
    -- style depends on state category and whether it is a selected state
    where (ls, as) = case sCategory $ getState gv id of
                          StateControllable   -> controllableStyle
                          StateUncontrollable -> uncontrollableStyle
                          StateBoth           -> bothStyle

stateAnnots :: GraphView c a -> a -> IO [GAnnotation]
stateAnnots gv srel = do
    staterels <- D.modelStateRels (gvModel gv)
    let supersets = map fst $ filter (\(n,r) -> (srel .-> r) == t) staterels
    return $ map (\n -> GAnnotation n AnnotRight stateAnnotStyle) supersets

transitionAnnots :: GraphView c a -> a -> IO [GAnnotation]
transitionAnnots gv tranrel = do
    tranrels <- D.modelTransRels (gvModel gv)
    let relnames = map fst $ filter (\(n,r) -> (srel .& r) /= b) tranrels
        labstr = minTermToStr $ D.tranMinTerm tranrel
    return $ GAnnotation labstr AnnotRight labelStyle
           : map (\n -> GAnnotation n AnnotLeft tranAnnotStyle) relnames
