module SourceView() where

import qualified Data.Graph.Inductive.Graph as Graph
import qualified Graphics.UI.Gtk            as G

import Util
import qualified DbgTypes as D
import NS
import qualified CFASpec  as C
import qualified IExpr    as I
import qualified CFA      as I

--------------------------------------------------------------
-- Data structures
--------------------------------------------------------------

-- Variable assignments
Store = SStruct [(String, Store)] -- name/value pairs (used for structs and for top-level store)
      | SArr    [Store]           -- array assignment
      | SVal    Maybe I.Val       -- scalar


storeGetLoc :: Store -> I.PID -> Maybe String -> I.Loc
storeGetLoc store pid methname = pcEnumToLoc pc
    where pcname = mkPCVarName $ pid ++ maybeToList methname
          pc     = storeGetScalar s pcname

data TraceEntry = TraceEntry I.Stack Store

type Trace = [TraceEntry]

data SourceView c a = SourceView {
    svSpec    :: C.Spec,
    svPID     :: I.PID,           -- PID set in the process selection menu
    svState   :: D.State a Store, -- Current state set via view callback
    svTrace   :: Trace            -- Steps from the current state
}

--------------------------------------------------------------
-- View callbacks
--------------------------------------------------------------

sourceViewNew :: (D.Rel c v a s) => C.Spec -> D.RModel c a Store -> IO (D.View a Store)
sourceViewNew spec model = do
    vbox <- G.vBoxNew False 0
    G.widgetShow vbox
    -- toolbar at the top
    tbar <- G.toolbarNew
    G.boxPackStart vbox tbar G.PackNatural 0
    G.widgetShow tbar
    -- process selector
    G.toolItemNew

    -- horizontal PanedPanels
    -- source window on the left
    -- stack, watch on the right
    ref = newIORef $ SourceView { svSpec  = spec
                                , svPID   = error "SourceView: PID undefined"
                                , svState = error "SourceView: state undefined"
                                , svTrace = []
                                }
    let cv = D.ViewEvents { D.evtStateSelected      = sourceViewStateSelected      ref 
                          , D.evtTransitionSelected = sourceViewTransitionSelected ref
                          }
    return $ D.View { D.viewName      = "Source"
                    , D.viewDefAlign  = D.AlignCenter
                    , D.viewShow      = return ()
                    , D.viewHide      = return ()
                    , D.viewGetWidget = return $ G.toWidget vbox
                    , D.viewCB        = cb
                    }
    

sourceViewStateSelected :: (D.Rel c v a s) => RSourceView c a -> Maybe (D.State a Store) -> IO ()
sourceViewStateSelected ref Nothing                              = disable ref
sourceViewStateSelected ref (Just s) | isNothing (D.sConcrete s) = disable ref
                                     | otherwise                 = do
    modifyIORef ref (\sv -> sv{svState = s})
    reset ref

sourceViewTransitionSelected :: (D.Rel c v a s) => RSourceView c a -> D.Transition a Store -> IO ()
    -- ignore abstract transitions
    -- run transition with automatic determinisation



--------------------------------------------------------------
-- Actions
--------------------------------------------------------------

-- Reset all components
reset :: RSourceView c a -> IO ()
reset ref = do
    sv <- readIORef ref
    -- initialise stack and trace
    let trace = [stackFromStore (D.sConcrete s) (svPID sv)]
    writeIORef ref sv{svTrace = trace}
    updateProcesses ref
    updateDisplays ref

-- Disable all controls
disable ref 

-- Execute one statement
step :: RSourceView c a -> IO Bool
step ref = do
    -- save current state
    backup <- readIORef ref
    ok <- microstep ref
    if ok
       then do -- did we reach the next statement?
               action <- getIORef (I.locAct . currentLocLabel) ref
               case action of
                    ActNone -> step ref
                    _       -> do -- if we reached a pause statement, update PC and PID variables 
                                  lab <- getIORef currentLocLabel ref
                                  when (isDelayLabel lab) $ updatePCPID ref
                                  updateDisplays ref
                                  return True 
       else do -- rollback changes
               writeIORef ref backup
               updateDisplays ref
               return False



-- run until pause or nondeterministic choice
run :: RSourceView c a -> IO ()
run ref = do
    ok <- step ref
    lab <- getIORef currentLocLabel ref
    when (ok && (not $ isDelayLabel lab)) $ run ref


--------------------------------------------------------------
-- Components
--------------------------------------------------------------


-- Stack --

-- Trace --

-- Watch --

-- Controls --

-- Source --
 

--------------------------------------------------------------
-- Private helpers
--------------------------------------------------------------

-- Given a snapshot of the store at a pause location, compute
-- process stack.
stackFromStore :: Store -> I.PID -> Stack
stackFromStore s pid = 
    -- concatenate stacks from each process down the branch of the process tree
    concat $ reverse $ map (\pid -> stackFromStore' s pid Nothing) $ tail $ inits pid

stackFromStore' :: Store -> I.PID -> Maybe String -> Stack
stackFromStore' s pid methname = stack' ++ stack
    where cfa    = C.getCFA pid methname
          loc    = storeGetLoc store pid methname
          label  = cfaLocLabel loc cfa
          stack  = locStack label
          -- If this location corresponds to a task call, recurse into task's CFA
          stack' = case label of 
                        LPause _ _ e -> case isWaitForTask e of
                                             Nothing   -> []
                                             Just name -> stackFromStore' s pid (Just name)
                        _            -> []


-- compute enabled processes and update process selector
updateProcesses

-- update all displays
updateDisplays
   updateStack
   updateTrace
   updateWatch
   updateSource


currentPID :: SourceView -> I.PID

currentCFA :: SourceView -> I.CFA

currentLoc :: SourceView -> I.Loc

currentLocLabel :: SourceView -> I.LocLabel

currentStore :: SourceView -> Store

currentStack :: SourceView -> I.Stack

-- Execute one CFA transition
-- Returns True if the step was performed successfully and
-- False otherwise (i.e., the user did not provide values
-- for nondeterministic arguments)
microstep :: RSourceView c a -> IO Bool
microstep ref = do
    sv <- readIORef ref
    -- Try all transitions from the current location; choose the first successful one
    let transitions = Graph.lsuc (currentCFA sv) (currencLoc sv)
    sv' <- foldM resolveTran sv transitions
    case mapMaybe (microstep' sv') transitions of
         []                      -> return False
         (Just (store, stack)):_ -> do writeIORef ref sv' $ traceAppend sv store stack
                                       return True

microstep' :: SourceView c a -> (I.Loc, I.TranLabel) -> Maybe (Store, I.Stack)
microstep' sv (to, TranCall scope)         = Just (currentStore sv, (Frame scope to) : currentStack sv)
microstep' sv (to, TranReturn)             = Just (currentStore sv, tail $ currentStack sv)
microstep' sv (to, TranNop)                = Just (currentStore sv, (head $ currentStack sv){fLoc = to} : (tail $ currentStack sv))
microstep' sv (to, TranStat (SAssume e))   = case eval e of
                                                  BoolVal True -> Just (currentStore sv, (head $ currentStack sv){fLoc = to} : (tail $ currentStack sv))
                                                  _            -> Nothing
microstep' sv (to, TranStat (SAssign l r)) = Just (currentStore sv, (head $ currentStack sv){fLoc = to} : (tail $ currentStack sv))
                                             where store' = storeSet (currentStore sv) l (eval r)

resolveTran :: SourceView c a -> (I.Loc, I.TranLabel) -> IO (SourceView c a)
resolveTran sv (_, TranStat (SAssume e))   = resolveExpr sv e
resolveTran sv (_, TranStat (SAssign l r)) = resolveExpr sv r
resolveTran sv _                           = return sv
