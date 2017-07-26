{-# LANGUAGE ScopedTypeVariables, ImplicitParams, RecordWildCards #-}

module Debug.GraphView(graphViewNew) where

import Data.IORef
import Data.List
import Data.Maybe
import Data.Tuple.Select
import Data.Bits
import Control.Monad
import qualified Graphics.UI.Gtk as Gt

import Util
import Implicit
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.Tree  as G
import qualified Debug.DbgTypes             as D
import qualified Debug.IDE                  as D
import GraphDraw

--------------------------------------------------------------
-- Constants
--------------------------------------------------------------

initLocation        = (200, 50)
childYOffset        = 80
graphSearchStep     = 30

tranAnnotStyle      =   GC {gcFG = (65535, 0, 0),         gcLW=0, gcLS=True}
stateAnnotStyle     =   GC {gcFG = (65535, 0, 0),         gcLW=0, gcLS=True}
labelStyle          =   GC {gcFG = (0, 0, 0),             gcLW=0, gcLS=True}

controllableStyle   =   GC {gcFG = (0    , 40000, 0),     gcLW=2, gcLS=True}
uncontrollableStyle =   GC {gcFG = (65535, 0    , 0),     gcLW=2, gcLS=True}
neutralStyle        =   GC {gcFG = (40000, 40000, 40000), gcLW=2, gcLS=True}

-- transitionStyle     =   GC {gcFG = (0, 40000, 0),         gcLW=2, gcLS=True}
subsetStyle         =   GC {gcFG = (40000, 40000, 40000), gcLW=1, gcLS=True}
overlapStyle        =   GC {gcFG = (40000, 40000, 40000), gcLW=2, gcLS=False}
eqStyle             =   GC {gcFG = (40000, 40000, 40000), gcLW=2, gcLS=True}

--controllableStyle   = ( GC {gcFG = (0    ,     0, 40000), gcLW=2, gcLS=True}
--                      , GC {gcFG = (0    , 65535, 0),     gcLW=0, gcLS=False})
--uncontrollableStyle = ( GC {gcFG = (0    ,     0, 40000), gcLW=2, gcLS=True}
--                      , GC {gcFG = (65535,     0, 0),     gcLW=0, gcLS=False})
defStateStyle       = ( GC {gcFG = (0    ,     0, 40000), gcLW=2, gcLS=True}
                      , GC {gcFG = (40000, 40000, 40000), gcLW=0, gcLS=False})

-- show concrete nodes as smaller circles
scaleConcreteNode   = 0.5

--------------------------------------------------------------
-- Types
--------------------------------------------------------------

data Edge a b d = EdgeTransition {eId :: Int, eTran :: D.Transition a b d}
                | EdgeSubset     {eId :: Int}
                | EdgeOverlap    {eId :: Int}
                | EdgeEq         {eId :: Int}

type TrGraph a b d = G.Gr (D.State a d) (Edge a b d)

data GraphView c a b d = GraphView {
    gvModel         :: D.RModel c a b d,
    gvGraphDraw     :: RGraphDraw,
    gvGraph         :: TrGraph a b d,
    gvSelectedState :: Maybe G.Node,
    gvSelectedTrans :: Maybe GEdgeId,
    gvLastEdgeId    :: GEdgeId
}

type RGraphView c a b d = IORef (GraphView c a b d)

--------------------------------------------------------------
-- View callbacks
--------------------------------------------------------------

graphViewNew :: (D.Rel c v a s, D.Vals b, D.Vals d) => D.RModel c a b d -> IO (D.View a b d)
graphViewNew model = do
    draw <- graphDrawNew
    ref <- newIORef $ GraphView { gvModel         = model
                                , gvGraphDraw     = draw
                                , gvGraph         = G.empty
                                , gvSelectedState = Nothing
                                , gvSelectedTrans = Nothing
                                , gvLastEdgeId    = 0
                                }
    graphDrawSetCB draw $ graphDrawDefaultCB { onEdgeLeftClick  = edgeLeftClick  ref
                                             , onNodeLeftClick  = nodeLeftClick  ref
                                             , onNodeRightClick = nodeRightClick ref
                                             }

    let cb = D.ViewEvents { D.evtStateSelected      = graphViewStateSelected      ref 
                          , D.evtTransitionSelected = graphViewTransitionSelected ref
                          , D.evtTRelUpdated        = return ()
                          }
    return $ D.View { D.viewName      = "Transition graph"
                    , D.viewDefAlign  = D.AlignCenter
                    , D.viewShow      = graphDrawConnect draw
                    , D.viewHide      = graphDrawDisconnect draw
                    , D.viewGetWidget = graphDrawWidget draw
                    , D.viewQuit      = return True
                    , D.viewCB        = cb
                    }

graphViewStateSelected :: (D.Rel c v a s, D.Vals b, D.Vals d) => RGraphView c a b d -> Maybe (D.State a d) -> IO ()
graphViewStateSelected ref mstate = do
    -- If selected state is equal to one of states in the graph, highlight this state
    gv <- readIORef ref
    ctx <- D.modelCtx $ gvModel gv
    let ?m = ctx
    gv1 <- case mstate of
                Nothing -> setSelectedState gv Nothing
                Just s  -> do (sid, gv') <- findOrCreateState gv Nothing s
                              setSelectedState gv' (Just sid)
    gv2 <- setSelectedTrans gv1 Nothing
    writeIORef ref gv2

graphViewTransitionSelected :: (D.Rel c v a s, D.Vals b, D.Vals d) => RGraphView c a b d -> D.Transition a b d -> IO ()
graphViewTransitionSelected ref tran = do
    gv <- readIORef ref
    model <- readIORef $ gvModel gv
    let ?m = D.mCtx model
    -- Find or create from-state
    (fromid, gv1) <- findOrCreateState gv (gvSelectedState gv) $ D.tranFrom tran
    -- Find or create to-state
    (toid, gv2)   <- findOrCreateState gv1 (Just fromid) $ D.tranTo tran
    -- Add transition
    (eid, gv3)    <- findOrCreateTransition gv2 fromid toid tran
    gv4 <- setSelectedState gv3 Nothing
    gv5 <- setSelectedTrans gv4 (Just eid)
    writeIORef ref gv5

--------------------------------------------------------------
-- GraphDraw callbacks
--------------------------------------------------------------

edgeLeftClick :: RGraphView c a b d -> (Int, Int, GEdgeId) -> IO ()
edgeLeftClick ref (_, _, eid) = do
    gv <- readIORef ref
    case findEdge gv eid of
         Just (EdgeTransition _ tran) -> D.modelSelectTransition (gvModel gv) tran
         _                            -> return ()
    
nodeLeftClick :: RGraphView c a b d -> GNodeId -> IO ()
nodeLeftClick ref nid = do
    gv <- readIORef ref
    D.modelSelectState (gvModel gv) (Just $ getState gv nid)

nodeRightClick :: (D.Rel c v a s, D.Vals b, D.Vals d) => RGraphView c a b d -> GNodeId -> IO ()
nodeRightClick ref nid = do
    gv <- readIORef ref
    let s = getState gv nid

    menu <- Gt.menuNew

    idelete <- Gt.menuItemNewWithLabel "Delete Node"
    _ <- Gt.on idelete Gt.menuItemActivate (deleteState ref nid)
    Gt.containerAdd menu idelete
    Gt.widgetShow idelete

    when (not $ D.isConcreteState s) $ do
        iconc <- Gt.menuItemNewWithLabel "Concretise Node"
        _ <- Gt.on iconc Gt.menuItemActivate (concretiseState ref nid)
        Gt.containerAdd menu iconc
        Gt.widgetShow iconc

    Gt.menuPopup menu Nothing

--------------------------------------------------------------
-- Private functions
--------------------------------------------------------------

findState :: (D.Rel c v a s, D.Vals d, ?m::c) => GraphView c a b d -> D.State a d -> Maybe G.Node
findState gv s = fmap fst $ find (D.eqStates s . snd) $ G.labNodes $ gvGraph gv

getState :: GraphView c a b d -> G.Node -> D.State a d
getState gv nid = fromJust $ G.lab (gvGraph gv) nid

findEdge :: GraphView c a b d -> GEdgeId -> Maybe (Edge a b d)
findEdge gv eid = fmap sel3 $ find ((==eid) . eId . sel3) $ G.labEdges $ gvGraph gv

findTransition :: (D.Rel c v a s, D.Vals b, D.Vals d, ?m::c) => GraphView c a b d -> G.Node -> G.Node -> D.Transition a b d -> Maybe GEdgeId
findTransition gv fromid toid tran = 
    fmap (\(_,_,e) -> eId e)
    $ find (\(fr,to,e) -> fr == fromid && to == toid &&
                          case e of 
                               EdgeTransition _ tran' -> D.eqTransitions tran' tran
                               _                      -> False) 
    $ G.labEdges $ gvGraph gv

getTransition :: GraphView c a b d -> GEdgeId -> D.Transition a b d
getTransition gv eid = tran
    where Just (EdgeTransition _ tran) = findEdge gv eid

setSelectedState :: (D.Rel c v a s, D.Vals b, ?m::c) => GraphView c a b d -> Maybe G.Node -> IO (GraphView c a b d)
setSelectedState gv mid = do
    let gv' = gv {gvSelectedState = mid}
    -- Deselect previous active node
    case gvSelectedState gv of
         Nothing  -> return ()
         Just sid -> graphDrawSetNodeStyle (gvGraphDraw gv) sid $ stateStyle gv' sid
    -- Set new selection
    case mid of
         Nothing  -> return ()
         Just sid -> graphDrawSetNodeStyle (gvGraphDraw gv) sid $ stateStyle gv' sid
    return gv'

setSelectedTrans :: (D.Rel c v a s, D.Vals b, ?m::c) => GraphView c a b d -> (Maybe GEdgeId) -> IO (GraphView c a b d)
setSelectedTrans gv mid = do
    let gv' = gv {gvSelectedTrans = mid}
    -- Deselect previous active edge
    case gvSelectedTrans gv of
         Nothing  -> return ()
         Just eid -> do style <- transitionStyle gv' $ getTransition gv' eid
                        graphDrawSetEdgeStyle (gvGraphDraw gv) eid style
    -- Set new selection
    case mid of
         Nothing  -> return ()
         Just eid -> do style <- transitionStyle gv' $ getTransition gv' eid
                        graphDrawSetEdgeStyle (gvGraphDraw gv) eid (style {gcLW = gcLW style + 2})
    return gv'

findOrCreateState :: (D.Rel c v a s, D.Vals b, D.Vals d, ?m::c) => GraphView c a b d -> Maybe G.Node -> D.State a d -> IO (G.Node, GraphView c a b d)
findOrCreateState gv mid s = 
    case findState gv s of
         Just sid -> return (sid, gv)
         Nothing  -> do coords <- chooseLocation gv mid
                        createState gv coords s

findOrCreateTransition :: (D.Rel c v a s, D.Vals b, D.Vals d, ?m::c) => GraphView c a b d -> G.Node -> G.Node -> D.Transition a b d -> IO (GEdgeId, GraphView c a b d)
findOrCreateTransition gv fromid toid tran = do
    case findTransition gv fromid toid tran of
         Just eid -> return (eid, gv)
         Nothing  -> createTransition gv fromid toid tran

createState :: (D.Rel c v a s, D.Vals b, ?m::c) => GraphView c a b d -> (Double, Double) -> D.State a d -> IO (G.Node, GraphView c a b d)
createState gv coords s = do
    let rel = D.sAbstract s
        sid = if G.order (gvGraph gv) == 0 then 0 else ((snd $ G.nodeRange (gvGraph gv)) + 1)
        gv' = gv {gvGraph = G.insNode (sid, s) (gvGraph gv)}
        (eqsets, other0)    = partition ((.== rel)           . D.sAbstract . getState gv) (G.nodes $ gvGraph gv)
        (subsets, other1)   = partition ((.== t) . (.-> rel) . D.sAbstract . getState gv) other0
        (supersets, other2) = partition ((.== t) . (rel .->) . D.sAbstract . getState gv) other1
        overlaps            = filter    ((./= b) . (.& rel)  . D.sAbstract . getState gv) other2
        rscale = if isNothing (D.sConcrete s) then 1.0 else scaleConcreteNode
        (ls, as) = stateStyle gv' sid
    annots <- stateAnnots gv' rel
    graphDrawInsertNode (gvGraphDraw gv') sid annots coords rscale (ls, as)
    gv0 <- foldM (\_gv sid' -> (liftM snd) $ createEqEdge      _gv sid  sid') gv' eqsets
    gv1 <- foldM (\_gv sid' -> (liftM snd) $ createSubsetEdge  _gv sid' sid)  gv0 subsets
    gv2 <- foldM (\_gv sid' -> (liftM snd) $ createSubsetEdge  _gv sid  sid') gv1 supersets
    gv3 <- foldM (\_gv sid' -> (liftM snd) $ createOverlapEdge _gv sid  sid') gv2 overlaps
    return (sid, gv3)

deleteState :: RGraphView c a b d -> G.Node -> IO ()
deleteState ref nid = do
    gv <- readIORef ref
    graphDrawDeleteNode (gvGraphDraw gv) nid
    writeIORef ref $ gv {gvGraph = G.delNode nid (gvGraph gv)}
    case gvSelectedTrans gv of
         Nothing  -> return ()
         Just eid -> do gv' <- readIORef ref
                        when (isNothing $ findEdge gv' eid) $ 
                             writeIORef ref $ gv' {gvSelectedTrans = Nothing}
    if Just nid == gvSelectedState gv
       then do modifyIORef ref $ \gv' -> gv' {gvSelectedState = Nothing}
               D.modelSelectState (gvModel gv) Nothing
       else return ()

concretiseState :: (D.Rel c v a s, D.Vals b, D.Vals d) => RGraphView c a b d -> G.Node -> IO ()
concretiseState ref nid = do
    gv <- readIORef ref
    ctx <- D.modelCtx $ gvModel gv
    let ?m = ctx
    let s = getState gv nid
    ms' <- D.modelConcretiseState (gvModel gv) (D.sAbstract s)
    case ms' of
         Nothing -> D.showMessage (gvModel gv) Gt.MessageError "Could not find concrete representative of the abstract state"
         Just s' -> do (nid', gv') <- findOrCreateState gv (Just nid) s'
                       writeIORef ref gv'
                       D.modelSelectState (gvModel gv') (Just $ getState gv' nid')

createTransition :: (D.Rel c v a s, D.Vals b, ?m::c) => GraphView c a b d -> G.Node -> G.Node -> D.Transition a b d -> IO (GEdgeId, GraphView c a b d)
createTransition gv fromid toid tran = do
    annots <- transitionAnnots gv tran
    style <- transitionStyle gv tran
    createEdge gv fromid toid (\eid -> EdgeTransition {eId = eid, eTran =  tran}) annots style EndArrow True

createSubsetEdge :: GraphView c a b d -> G.Node -> G.Node -> IO (GEdgeId, GraphView c a b d)
createSubsetEdge gv fromid toid = 
    createEdge gv fromid toid EdgeSubset [] subsetStyle EndDiamond False 

createEqEdge :: GraphView c a b d -> G.Node -> G.Node -> IO (GEdgeId, GraphView c a b d)
createEqEdge gv fromid toid = 
    createEdge gv fromid toid EdgeEq [] eqStyle EndNone False

createOverlapEdge :: GraphView c a b d -> G.Node -> G.Node -> IO (GEdgeId, GraphView c a b d)
createOverlapEdge gv fromid toid = 
    createEdge gv fromid toid EdgeOverlap [] overlapStyle EndNone False

createEdge :: GraphView c a b d -> G.Node -> G.Node -> (GEdgeId -> Edge a b d) -> [GAnnotation] -> GC -> EndStyle -> Bool -> IO (GEdgeId, GraphView c a b d)
createEdge gv fromid toid f annots gc end visible = do
    let eid = gvLastEdgeId gv + 1
        e = f eid
        gv' = gv { gvGraph      = G.insEdge (fromid, toid, e) (gvGraph gv)
                 , gvLastEdgeId = eid}
    graphDrawInsertEdge (gvGraphDraw gv') fromid toid eid annots gc end visible
    return (eid, gv')

chooseLocation :: GraphView c a b d -> Maybe G.Node -> IO (Double, Double)
chooseLocation gv Nothing = chooseLocationFrom gv initLocation
chooseLocation gv (Just sid) = do
    (parentx, parenty) <- graphDrawGetNodeCoords (gvGraphDraw gv) sid
    chooseLocationFrom gv (parentx, parenty + childYOffset)

-- Find unoccupied location next to given coordinates
chooseLocationFrom :: GraphView c a b d -> (Double, Double) -> IO (Double, Double)
chooseLocationFrom gv (x,y) = do
    ids <- graphDrawGetNodesAtLocation (gvGraphDraw gv) (x,y)
    case ids of
         [] -> return (x,y)
         _  -> chooseLocationFrom gv (x,y+graphSearchStep)

stateStyle :: (D.Rel c v a s, D.Vals b, ?m::c) => GraphView c a b d -> G.Node -> (GC, GC)
stateStyle gv sid = if Just sid == gvSelectedState gv
                       then (ls{gcLW = gcLW ls + 1}, as{gcFG = map3 ((`shiftR` 1),(`shiftR` 1),(`shiftR` 1)) $ gcFG as})
                       else (ls,as)
    where (ls, as) = defStateStyle

stateAnnots :: (D.Rel c v a s, ?m::c) => GraphView c a b d -> a -> IO [GAnnotation]
stateAnnots gv srel = do
    staterels <- D.modelStateRels (gvModel gv)
    let supersets = map fst 
                    $ filter (\(_,r) -> (srel .-> r) .== t) 
                    staterels 
    return $ map (\n -> GAnnotation n AnnotRight stateAnnotStyle) supersets

transitionStyle :: (D.Rel c v a s, D.Vals b, ?m::c) => GraphView c a b d -> D.Transition a b d -> IO GC
transitionStyle gv tran = do
    model <- readIORef $ gvModel gv
    cat <- D.transitionCategory model tran
    return $ case cat of
                  D.TranControllable   -> controllableStyle
                  D.TranUncontrollable -> uncontrollableStyle
                  _                    -> neutralStyle
  
transitionAnnots :: (D.Rel c v a s, ?m::c) => GraphView c a b d -> D.Transition a b d -> IO [GAnnotation]
transitionAnnots gv tran = do
    model <- readIORef $ gvModel gv
    let tranrels = D.mTransRels model
        relnames = map sel1 $ filter (\(_,_,r) -> (D.tranRel model tran .& r) ./= b) tranrels
    labstr <- case D.tranSrc tran of
                   Nothing  -> transitionLabel (gvModel gv) (D.tranAbstractLabel tran)
                   Just str -> return str
    return $ GAnnotation labstr AnnotRight labelStyle
           : map (\n -> GAnnotation n AnnotLeft tranAnnotStyle) relnames

transitionLabel :: (D.Rel c v a s, ?m::c) => D.RModel c a b d -> a -> IO String
transitionLabel rmodel rel = do
    lvars <- D.modelLabelVars rmodel
    let vals = maybe [] (map (\(v,i) -> (v, D.valStrFromInt (D.mvarType v) i))) (D.oneSatVal rel lvars)
    return $ intercalate "," $ map (\(var,val) -> D.mvarName var ++ "=" ++ val) vals
