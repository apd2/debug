{-# LANGUAGE ScopedTypeVariables, ImplicitParams, RecordWildCards #-}

module GraphView(graphViewNew) where

import Data.IORef
import Data.List
import Data.Maybe
import Data.Bits
import Control.Monad

import Util
import Implicit
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.Tree  as G
import qualified DbgTypes                   as D
import qualified IDE                        as D
import GraphDraw

--------------------------------------------------------------
-- Constants
--------------------------------------------------------------

initLocation        = (200, 50)
childYOffset        = 50
graphSearchStep     = 30

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

--------------------------------------------------------------
-- Types
--------------------------------------------------------------

data StateCategory = StateControllable
                   | StateUncontrollable
                   | StateBoth

data State a = State {
    sState    :: a,
    sCategory :: StateCategory
}

data Edge a = EdgeTransition {eId :: Int, eTran :: D.Transition a}
            | EdgeSubset     {eId :: Int}
            | EdgeOverlap    {eId :: Int}

type TrGraph a = G.Gr (State a) (Edge a)

data GraphView c a = GraphView {
    gvModel         :: D.RModel c a,
    gvGraphDraw     :: RGraphDraw,
    gvGraph         :: TrGraph a,
    gvSelectedState :: Maybe G.Node,
    gvSelectedTrans :: Maybe GEdgeId,
    gvLastEdgeId    :: GEdgeId
}

type RGraphView c a = IORef (GraphView c a)

--------------------------------------------------------------
-- View callbacks
--------------------------------------------------------------

graphViewNew :: (D.Rel c v a s) => D.RModel c a -> IO (D.View a)
graphViewNew model = do
    draw <- graphDrawNew
    ref <- newIORef $ GraphView { gvModel         = model
                                , gvGraphDraw     = draw
                                , gvGraph         = G.empty
                                , gvSelectedState = Nothing
                                , gvSelectedTrans = Nothing
                                , gvLastEdgeId    = 0
                                }
    graphDrawSetCB draw $ graphDrawDefaultCB { onEdgeLeftClick = edgeLeftClick ref
                                             , onNodeLeftClick = nodeLeftClick ref}

    let cb = D.ViewEvents { D.evtStateSelected      = graphViewStateSelected      ref 
                          , D.evtTransitionSelected = graphViewTransitionSelected ref
                          }
    return $ D.View { D.viewName      = "Transition graph"
                    , D.viewDefAlign  = D.AlignCenter
                    , D.viewShow      = graphDrawConnect draw
                    , D.viewHide      = graphDrawDisconnect draw
                    , D.viewGetWidget = graphDrawWidget draw
                    , D.viewCB        = cb
                    }

graphViewStateSelected :: (D.Rel c v a s) => RGraphView c a -> Maybe a -> IO ()
graphViewStateSelected ref mrel = do
    -- If selected state is equal to one of states in the graph, highlight this state
    gv <- readIORef ref
    ctx <- D.modelCtx $ gvModel gv
    let ?m = ctx
    gv1 <- case mrel of
                Nothing  -> setSelectedState gv Nothing
                Just rel -> do (id, gv') <- findOrCreateState gv Nothing rel
                               setSelectedState gv' (Just id)
    gv2 <- setSelectedTrans gv1 Nothing
    writeIORef ref gv2

graphViewTransitionSelected :: (D.Rel c v a s) => RGraphView c a -> D.Transition a -> IO ()
graphViewTransitionSelected ref tran = do
    gv <- readIORef ref
    model <- readIORef $ gvModel gv
    let ?m = D.mCtx model
    -- Find or create from-state
    (fromid, gv1) <- findOrCreateState gv (gvSelectedState gv) $ D.tranFrom tran
    -- Find or create to-state
    (toid, gv2)   <- findOrCreateState gv1 (Just fromid) $ D.tranTo model tran
    -- Add transition
    (eid, gv3)      <- findOrCreateTransition gv2 fromid toid tran
    gv4 <- setSelectedState gv3 Nothing
    gv5 <- setSelectedTrans gv4 (Just eid)
    writeIORef ref gv5

--------------------------------------------------------------
-- GraphDraw callbacks
--------------------------------------------------------------

edgeLeftClick :: RGraphView c a -> (Int, Int, GEdgeId) -> IO ()
edgeLeftClick ref (from, to, id) = do
    gv <- readIORef ref
    case findEdge gv id of
         Just (EdgeTransition _ tran) -> D.modelSelectTransition (gvModel gv) tran
         _                            -> return ()
    
nodeLeftClick :: RGraphView c a -> GNodeId -> IO ()
nodeLeftClick ref id = do
    gv <- readIORef ref
    D.modelSelectState (gvModel gv) (Just $ sState $ getState gv id)


--------------------------------------------------------------
-- Private functions
--------------------------------------------------------------

-- TODO: Potentially, move this function to DbgTypes.hs
stateCategory :: (D.Rel c v a s, ?m::c) => D.RModel c a -> a -> IO StateCategory
stateCategory rmodel rel = do
    srels <- D.modelStateRels rmodel
    case find ((==D.contRelName) . fst) srels of
         Nothing        -> return StateBoth
         Just (_, cont) -> if rel `leq` cont
                              then return StateControllable
                              else if rel `leq` nt cont
                                      then return StateUncontrollable
                                      else return StateBoth


findState :: (D.Rel c v a s, ?m::c) => GraphView c a -> a -> Maybe G.Node
findState gv rel = fmap fst $ find (\(id, s) -> sState s == rel) $ G.labNodes $ gvGraph gv

getState :: GraphView c a -> G.Node -> State a
getState gv id = fromJust $ G.lab (gvGraph gv) id

findEdge :: GraphView c a -> Int -> Maybe (Edge a)
findEdge gv id = fmap trd3 $ find ((==id) . eId . trd3) $ G.labEdges $ gvGraph gv

findTransition :: (D.Rel c v a s, ?m::c) => GraphView c a -> G.Node -> G.Node -> a -> Maybe GEdgeId
findTransition gv fromid toid labrel = 
    fmap (\(_,_,e) -> eId e)
    $ find (\(f,t,e) -> f == fromid && t == toid &&
                        case e of 
                             EdgeTransition _ t -> D.tranLabel t == labrel
                             _                  -> False) 
    $ G.labEdges $ gvGraph gv

setSelectedState :: GraphView c a -> (Maybe G.Node) -> IO (GraphView c a)
setSelectedState gv mid = do
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

setSelectedTrans :: GraphView c a -> (Maybe GEdgeId) -> IO (GraphView c a)
setSelectedTrans gv mid = do
    let gv' = gv {gvSelectedTrans = mid}
    -- Deselect previous active node
    case gvSelectedTrans gv of
         Nothing -> return ()
         Just id -> graphDrawSetEdgeStyle (gvGraphDraw gv) id transitionStyle
    -- Set new selection
    case mid of
         Nothing -> return ()
         Just id -> graphDrawSetEdgeStyle (gvGraphDraw gv) id (transitionStyle {gcLW = gcLW transitionStyle + 2})
    return gv'

findOrCreateState :: (D.Rel c v a s, ?m::c) => GraphView c a -> Maybe G.Node -> a -> IO (G.Node, GraphView c a)
findOrCreateState gv mid rel = 
    case findState gv rel of
         Just id -> return (id, gv)
         Nothing -> do coords <- chooseLocation gv mid
                       createState gv coords rel

findOrCreateTransition :: (D.Rel c v a s, ?m::c) => GraphView c a -> G.Node -> G.Node -> D.Transition a -> IO (GEdgeId, GraphView c a)
findOrCreateTransition gv fromid toid tran = do
    case findTransition gv fromid toid (D.tranLabel tran) of
         Just id -> return (id, gv)
         Nothing -> createTransition gv fromid toid tran

createState :: (D.Rel c v a s, ?m::c) => GraphView c a -> (Double, Double) -> a -> IO (G.Node, GraphView c a)
createState gv coords rel = do
    cat <- stateCategory (gvModel gv) rel
    let id = (snd $ G.nodeRange (gvGraph gv)) + 1
        st = State {sState = rel, sCategory = cat}
        gv' = gv {gvGraph = G.insNode (id, st) (gvGraph gv)}
        (ls, as) = stateStyle gv' id
        (subsets, other1)   = partition ((== t) . (.-> rel) . sState . getState gv) (G.nodes $ gvGraph gv)
        (supersets, other2) = partition ((== t) . (rel .->) . sState . getState gv) other1
        overlaps            = filter    ((/= b) . (.& rel)  . sState . getState gv) other2
    annots <- stateAnnots gv' rel
    graphDrawInsertNode (gvGraphDraw gv') id annots coords (ls, as)
    gv1 <- foldM (\gv sid -> (liftM snd) $ createSubsetEdge  gv sid id) gv' subsets
    gv2 <- foldM (\gv sid -> (liftM snd) $ createSubsetEdge  gv id sid) gv1 supersets
    gv3 <- foldM (\gv sid -> (liftM snd) $ createOverlapEdge gv id sid) gv2 overlaps
    return (id, gv3)

createTransition :: (D.Rel c v a s, ?m::c) => GraphView c a -> G.Node -> G.Node -> D.Transition a -> IO (GEdgeId, GraphView c a)
createTransition gv fromid toid tran = do
    annots <- transitionAnnots gv tran
    createEdge gv fromid toid (\id -> EdgeTransition id tran) annots transitionStyle EndArrow True

createSubsetEdge :: GraphView c a -> G.Node -> G.Node -> IO (GEdgeId, GraphView c a)
createSubsetEdge gv fromid toid = 
    createEdge gv fromid toid EdgeSubset [] subsetStyle EndDiamond True

createOverlapEdge :: GraphView c a -> G.Node -> G.Node -> IO (GEdgeId, GraphView c a)
createOverlapEdge gv fromid toid = 
    createEdge gv fromid toid EdgeOverlap [] overlapStyle EndNone True

createEdge :: GraphView c a -> G.Node -> G.Node -> (GEdgeId -> Edge a) -> [GAnnotation] -> GC -> EndStyle -> Bool -> IO (GEdgeId, GraphView c a)
createEdge gv fromid toid f annots gc end visible = do
    let id = gvLastEdgeId gv + 1
        e = f id
        gv' = gv { gvGraph      = G.insEdge (fromid, toid, e) (gvGraph gv)
                 , gvLastEdgeId = id}
    graphDrawInsertEdge (gvGraphDraw gv') fromid toid id annots gc end visible
    return (id, gv')

chooseLocation :: GraphView c a -> Maybe G.Node -> IO (Double, Double)
chooseLocation gv Nothing = chooseLocationFrom gv initLocation
chooseLocation gv (Just id) = do
    (parentx, parenty) <- graphDrawGetNodeCoords (gvGraphDraw gv) id
    chooseLocationFrom gv (parentx, parenty + childYOffset)

-- Find unoccupied location next to given coordinates
chooseLocationFrom :: GraphView c a -> (Double, Double) -> IO (Double, Double)
chooseLocationFrom gv (x,y) = do
    ids <- graphDrawGetNodesAtLocation (gvGraphDraw gv) (x,y)
    case ids of
         [] -> return (x,y)
         _  -> chooseLocationFrom gv (x,y+graphSearchStep)

stateStyle :: GraphView c a -> G.Node -> (GC, GC)
stateStyle gv id = 
    if Just id == gvSelectedState gv
       then (ls{gcLW = gcLW ls + 1}, as{gcFG = map3 ((`shiftR` 1),(`shiftR` 1),(`shiftR` 1)) $ gcFG as})
       else (ls,as)
    -- style depends on state category and whether it is a selected state
    where (ls, as) = case sCategory $ getState gv id of
                          StateControllable   -> controllableStyle
                          StateUncontrollable -> uncontrollableStyle
                          StateBoth           -> bothStyle

stateAnnots :: (D.Rel c v a s, ?m::c) => GraphView c a -> a -> IO [GAnnotation]
stateAnnots gv srel = do
    staterels <- D.modelStateRels (gvModel gv)
    let supersets = map fst $ filter (\(n,r) -> (srel .-> r) == t) staterels
    return $ map (\n -> GAnnotation n AnnotRight stateAnnotStyle) supersets

transitionAnnots :: (D.Rel c v a s, ?m::c) => GraphView c a -> D.Transition a -> IO [GAnnotation]
transitionAnnots gv tran = do
    tranrels <- D.modelTransRels (gvModel gv)
    let relnames = map fst $ filter (\(n,r) -> (D.tranLabel tran .& r) /= b) tranrels
    labstr <- transitionLabel (gvModel gv) (D.tranLabel tran)
    return $ GAnnotation labstr AnnotRight labelStyle
           : map (\n -> GAnnotation n AnnotLeft tranAnnotStyle) relnames

transitionLabel :: (D.Rel c v a s, ?m::c) => D.RModel c a -> a -> IO String
transitionLabel rmodel rel = do
    -- variables in support
    let support = supportIndices rel
        asn     = fromJust $ oneSat (vconcat $ map varAtIndex support) rel
    lvars <- D.modelLabelVars rmodel
    let supvars = filter (any (\idx -> elem idx support) . trd3) lvars
        vals = map (\(_,t,ind) -> D.valStrFromInt t $ boolArrToBitsBe $ extract (D.idxToVS ind) asn) supvars
    return $ intercalate "," $ map (\(var,val) -> fst3 var ++ "=" ++ val) $ zip supvars vals
