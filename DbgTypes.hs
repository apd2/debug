{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, ImplicitParams, FunctionalDependencies, UndecidableInstances #-}

module DbgTypes(Rel,
                Vals,
                View(..),
                ViewEvents(..),
                Type(..),
                State(..),
                StateCategory(..),
                stateCategory,
                Transition(..),
                tranTo',
                tranRel,
                ModelVar(..),
                Model(..),
                RModel,
                mCurStateVars,
                mNextStateVars,
                mStateV, 
                mNextV, 
                mUntrackedV, 
                mLabelV,
                contRelName,
                idxToVS,
                valStrFromInt,
                oneSatVal,
                modelCtx,
                modelStateVars,
                modelLabelVars,
                modelUntrackedVars,
                modelStateRels,
                modelTransRels,
                modelActiveTransRel,
                modelSelectTransition,
                modelSelectState) where

import qualified Graphics.UI.Gtk as G 
import Data.IORef
import Data.List
import Control.Monad

import Util
import IDE
import qualified LogicClasses as L
import Implicit

------------------------------------------------------
-- Constants
------------------------------------------------------

contRelName = "Controllable states"

------------------------------------------------------
-- Types
------------------------------------------------------

class (L.Variable c v, 
       L.VarDecl c v,
       L.Shiftable c v a, 
       L.QBF c v a, 
       L.EqConst c v a, 
       L.Serialisable c a, 
       L.Satisfiable c v a s [Bool], 
       L.BoolOp c v a, 
       L.EqRaw c v a [Bool],
       L.CUDDLike c v a,
       L.Cubeable c v a,
       Show a) => Rel c v a s | c -> v, c -> a, c -> s

-- Concrete variable valuation
class (Eq b) => Vals b

idxToVS :: (Rel c v a s, ?m::c) => [Int] -> v
idxToVS indices = vconcat $ map varAtIndex indices

-- Debugger's own type system
data Type = Bool
          | SInt Int
          | UInt Int
          | Enum [String]

instance Show Type where
    show Bool     = "bool"
    show (SInt i) = "sint<" ++ show i ++ ">"
    show (UInt i) = "uint<" ++ show i ++ ">"
    show (Enum _) = "enum"

valStrFromInt :: Type -> Integer -> String
valStrFromInt Bool      0                                = "false"
valStrFromInt Bool      1                                = "true"
valStrFromInt (Enum es) i | length es >= fromInteger i+1 = es !! (fromInteger i)
                          | otherwise                    = "?"
valStrFromInt _         i                                = show i

-- View interface
data View a b = View {
    viewName      :: String,
    viewDefAlign  :: IDEAlign,
    viewShow      :: IO (),
    viewHide      :: IO (),
    viewGetWidget :: IO G.Widget,
    viewCB        :: ViewEvents a b
}

-- Events sent from the debugger core to each view
data ViewEvents a b = ViewEvents {
     evtStateSelected      :: Maybe (State a b) -> IO (),
     evtTransitionSelected :: Transition a b -> IO ()
}

data StateCategory = StateControllable
                   | StateUncontrollable
                   | StateBoth

data State a b = State {
    sAbstract :: a,      -- abstract state
    sConcrete :: Maybe b -- concrete state
}

instance (?m::c, L.Boolean c a, Eq b) => Eq (State a b) where
    (==) x y = sAbstract x .== sAbstract y && sConcrete x == sConcrete y

stateCategory :: (Rel c v a s, ?m::c) => Model c a b -> a -> IO StateCategory
stateCategory model rel = 
    case find ((==contRelName) . fst) (mStateRels model) of
         Nothing        -> return StateBoth
         Just (_, cont) -> if' (rel `leq` cont) (return StateControllable) $
                           if' (rel `leq` nt cont) (return StateUncontrollable) $
                           return StateBoth

data Transition a b = Transition {
    tranFrom          :: State a b,
    tranUntracked     :: a,
    tranAbstractLabel :: a,
    tranConcreteLabel :: Maybe b,
    tranTo            :: State a b
}

instance (?m::c, L.Boolean c a, Eq b) => Eq (Transition a b) where
    (==) x y =  tranFrom x == tranFrom y
             && tranTo x == tranTo y
             && tranUntracked x .== tranUntracked y
             && tranAbstractLabel x .== tranAbstractLabel y
             && tranConcreteLabel x == tranConcreteLabel y

-- Project next state to current state vars
tranTo' :: (Rel c v a s, ?m::c) => Model c a b -> Transition a b -> a
tranTo' model tr = swap (mNextV model) (mStateV model) (sAbstract $ tranTo tr)

-- Conjunction of state, next, label, and untracked relations
tranRel :: (Rel c v a s, ?m::c) => Model c a b -> Transition a b -> a
tranRel model tr = (sAbstract $ tranFrom tr) 
                .& (tranTo' model tr)
                .& (tranUntracked tr)
                .& (tranAbstractLabel tr)

data ModelVar = ModelVar { mvarName :: String
                         , mvarType :: Type
                         , mvarIdx  ::[Int]
                         }

-- Debugger state
data Model c a b = Model {
    -- Static part --
    mCtx                  :: c,

    -- Abstract variable sections
    mStateVars            :: [(String, Type, ([Int],[Int]))],
    mUntrackedVars        :: [ModelVar],
    mLabelVars            :: [ModelVar],

    -- State and transition relations being debugged
    mStateRels            :: [(String, a)],
    mTransRels            :: [(String, a)],

    -- Dynamic part --
    mViews                :: [View a b]
}

mCurStateVars :: Model c a b -> [ModelVar]
mCurStateVars = map (\(n,tp,(i,_)) -> ModelVar n tp i) . mStateVars

mNextStateVars :: Model c a b -> [ModelVar]
mNextStateVars = map (\(n,tp,(_,i)) -> ModelVar n tp i) . mStateVars

mStateV, mNextV, mUntrackedV, mLabelV :: (Rel c v a s, ?m::c) => Model c a b -> v
mStateV     = vconcat . map varAtIndex . concatMap mvarIdx . mCurStateVars
mNextV      = vconcat . map varAtIndex . concatMap mvarIdx . mNextStateVars
mUntrackedV = vconcat . map varAtIndex . concatMap mvarIdx . mUntrackedVars
mLabelV     = vconcat . map varAtIndex . concatMap mvarIdx . mLabelVars


type RModel c a b = IORef (Model c a b)


----------------------------------------------------------
-- External interface
----------------------------------------------------------

-- Querying state
modelCtx :: RModel c a b -> IO c
modelCtx ref = (liftM mCtx) $ readIORef ref

modelStateVars :: RModel c a b -> IO [(String, Type, ([Int],[Int]))]
modelStateVars ref = getIORef mStateVars ref

modelUntrackedVars :: RModel c a b -> IO [ModelVar]
modelUntrackedVars ref = getIORef mUntrackedVars ref

modelLabelVars :: RModel c a b -> IO [ModelVar]
modelLabelVars ref = getIORef mLabelVars ref

modelTransRels :: RModel c a b -> IO [(String, a)]
modelTransRels ref = getIORef mTransRels ref

modelStateRels :: RModel c a b -> IO [(String, a)]
modelStateRels ref = getIORef mStateRels ref

-- TODO: implement proper selection of transition relation to debug
modelActiveTransRel :: RModel c a b -> IO a
modelActiveTransRel ref = getIORef (snd . head . mTransRels) ref

-- Actions
modelSelectTransition :: RModel c a b -> Transition a b -> IO ()
modelSelectTransition ref tran = do
   views <- modelViews ref
   _ <- mapM (\v -> (evtTransitionSelected $ viewCB v) tran) views
   return ()

modelSelectState :: RModel c a b -> Maybe (State a b) -> IO ()
modelSelectState ref mrel = do
   views <- modelViews ref
   _ <- mapM (\v -> (evtStateSelected $ viewCB v) mrel) views
   return ()

-- Utils

-- Find satisfying assignment and return valuation of variables 
-- in support of rel
oneSatVal :: (Rel c v a s, ?m::c) => a -> [ModelVar] -> Maybe [(ModelVar, Integer)]
oneSatVal rel vars = do
    let support = supportIndices rel
    asn <- satOne rel
    let supvars = filter (any (\idx -> elem idx support) . mvarIdx) vars
    return $ map (\v -> (v, boolArrToBitsBe $ extract (idxToVS (mvarIdx v)) asn)) supvars

----------------------------------------------------------
-- Private functions
----------------------------------------------------------

modelViews :: RModel c a b -> IO [View a b]
modelViews ref = (liftM mViews) $ readIORef ref
