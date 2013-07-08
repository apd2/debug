{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, ImplicitParams, FunctionalDependencies, UndecidableInstances, RecordWildCards #-}

module DbgTypes(Rel,
                Vals,
                View(..),
                ViewEvents(..),
                Type(..),
                State(..),
                isConcreteState,
                StateCategory(..),
                stateCategory,
                Transition(..),
                tranTo',
                tranRel,
                isConcreteTransition,
                ModelVar(..),
                ModelStateVar(..),
                Model(..),
                RModel,
                mCurStateVars,
                mNextStateVars,
                mStateV, 
                mNextV, 
                mUntrackedV, 
                mLabelV,
                mDumpIndices,
                mUpdateTRel,
                contRelName,
                idxToVS,
                mvarToVS,
                valStrFromInt,
                valStrFromRel,
                isEnVarName,
                mkEnVarName,
                mkBaseVarName,
                oneSatVal,
                modelCtx,
                modelStateVars,
                modelLabelVars,
                modelUntrackedVars,
                modelStateRels,
                modelTransRels,
                modelConcretiseState,
                modelActiveTransRel,
                modelSelectTransition,
                modelSelectState,
                modelSetConstraint,
                showMessage) where

import qualified Graphics.UI.Gtk as G 
import Data.IORef
import Data.List
import Data.Maybe
import qualified Data.Map        as M
import Control.Monad

import Util
import IDE
import qualified LogicClasses    as L
import Implicit

------------------------------------------------------
-- Constants
------------------------------------------------------

contRelName = "controllable"

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
idxToVS = vconcat . map varAtIndex . sort

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

valStrFromRel :: (Rel c v a s, ?m::c) => v -> Type -> a -> String
valStrFromRel _ _  rel | rel .== t = "*"
valStrFromRel _ _  rel | rel .== b = "#"
valStrFromRel x tp rel             = valStrFromInt tp $ boolArrToBitsBe $ extract x $ fromJust $ satOne rel

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
     evtTransitionSelected :: Transition a b -> IO (),
     evtTRelUpdated        :: IO ()
}

data StateCategory = StateControllable
                   | StateUncontrollable
                   | StateBoth

data State a b = State {
    sAbstract :: a,      -- abstract state
    sConcrete :: Maybe b -- concrete state
}

isConcreteState :: State a b -> Bool
isConcreteState = isJust . sConcrete


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

isConcreteTransition :: Transition a b -> Bool
isConcreteTransition Transition{..} = isConcreteState tranFrom 
                                   && isConcreteState tranTo 
                                   && isJust tranConcreteLabel

data ModelVar = ModelVar { mvarName :: String
                         , mvarType :: Type
                         , mvarIdx  ::[Int]
                         }

type ModelStateVar = (String, Type, ([Int],[Int]))

-- Debugger state
data Model c a b = Model {
    -- Static part --
    mCtx                  :: c,

    -- Abstract variable sections
    mStateVars            :: [ModelStateVar],
    mUntrackedVars        :: [ModelVar],
    mLabelVars            :: [ModelVar],

    -- State and transition relations being debugged
    mStateRels            :: [(String, a)],
    mTransRels            :: [(String, a)],

    -- Callbacks
    mConcretiseState      :: a              -> Maybe (State a b),
    mConcretiseTransition :: Transition a b -> Maybe (Transition a b),

    -- Dynamic part --
    mViews                :: [View a b],
    mAutoConcretiseTrans  :: Bool,
    mConstraints          :: M.Map String a,
    mTransRel             :: a
}

mCurStateVars :: Model c a b -> [ModelVar]
mCurStateVars = map (\(n,tp,(i,_)) -> ModelVar n tp i) . mStateVars

mNextStateVars :: Model c a b -> [ModelVar]
mNextStateVars = map (\(n,tp,(_,i)) -> ModelVar n tp i) . mStateVars

mvarToVS :: (Rel c v a s, ?m::c) => ModelVar -> v
mvarToVS = idxToVS . mvarIdx

mStateV, mNextV, mUntrackedV, mLabelV :: (Rel c v a s, ?m::c) => Model c a b -> v
mStateV     = idxToVS . concatMap mvarIdx . mCurStateVars
mNextV      = idxToVS . concatMap mvarIdx . mNextStateVars
mUntrackedV = idxToVS . concatMap mvarIdx . mUntrackedVars
mLabelV     = idxToVS . concatMap mvarIdx . mLabelVars

mSetConstraint :: (Rel c v a s) => Model c a b -> String -> Maybe a -> Model c a b
mSetConstraint m cname Nothing  = mUpdateTRel $ m {mConstraints = M.delete cname   $ mConstraints m}
mSetConstraint m cname (Just r) = mUpdateTRel $ m {mConstraints = M.insert cname r $ mConstraints m}

mUpdateTRel :: (Rel c v a s) => Model c a b -> Model c a b
mUpdateTRel m@Model{..} = m {mTransRel = r}
    where r = let ?m = mCtx in 
              conj $ (snd $ head mTransRels) : (map snd $ M.toList mConstraints)

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

modelConcretiseState :: RModel c a b -> a -> IO (Maybe (State a b))
modelConcretiseState ref x = getIORef ((flip mConcretiseState) x) ref

modelActiveTransRel :: RModel c a b -> IO a
modelActiveTransRel ref = getIORef mTransRel ref

-- Actions
modelSelectTransition :: RModel c a b -> Transition a b -> IO ()
modelSelectTransition ref tran = do
   Model{..} <- readIORef ref
   let tran' = if (not $ isConcreteTransition tran) && mAutoConcretiseTrans
                  then case mConcretiseTransition tran of 
                            Nothing -> tran
                            Just tr -> tr
                  else tran
   _ <- mapM (\v -> (evtTransitionSelected $ viewCB v) tran')                 mViews
   _ <- mapM (\v -> (evtStateSelected      $ viewCB v) (Just $ tranTo tran')) mViews
   return ()

modelSelectState :: RModel c a b -> Maybe (State a b) -> IO ()
modelSelectState ref mrel = do
   views <- modelViews ref
   _ <- mapM (\v -> (evtStateSelected $ viewCB v) mrel) views
   return ()

modelSetConstraint :: (Rel c v a s) => RModel c a b -> String -> Maybe a -> IO ()
modelSetConstraint ref cname crel = do 
    modifyIORef ref $ \m -> mSetConstraint m cname crel
    views <- modelViews ref
    _ <- mapM (evtTRelUpdated . viewCB) views
    return ()

-- Utils

isEnVarName :: String -> Bool
isEnVarName = isSuffixOf ".en"

mkEnVarName :: String -> String
mkEnVarName = (++ ".en")

mkBaseVarName :: String -> String
mkBaseVarName n = take (length n - length ".en") n

-- Find satisfying assignment and return valuation of variables
-- in support of rel.  Filter out variables whose corresponsing 
-- .en variables are false.
oneSatVal :: (Rel c v a s, ?m::c) => a -> [ModelVar] -> Maybe [(ModelVar, Integer)]
oneSatVal rel vars = do
    let support = supportIndices rel
    asn <- satOne rel
    let supvars = filter (any (\idx -> elem idx support) . mvarIdx) vars
    return $ map (\v -> (v, boolArrToBitsBe $ extract (mvarToVS v) asn)) supvars

-- Message boxes
showMessage :: RModel c a b -> G.MessageType -> String -> IO ()
showMessage _ mtype mtext = do
    dialog <- G.messageDialogNew Nothing [G.DialogModal] mtype G.ButtonsOk mtext
    _ <- G.onResponse dialog (\_ -> G.widgetDestroy dialog)
    G.windowPresent dialog

----------------------------------------------------------
-- Debugging
----------------------------------------------------------

mDumpIndices :: Model c a b -> String
mDumpIndices m =
    intercalate "\n" $
    map (\(n,_,(i,i')) -> (pad p ' ' n) ++ ": (" ++ showi i ++ "," ++ showi i' ++ ")") (mStateVars     m) ++
    map showmv                                                                         (mUntrackedVars m) ++
    map showmv                                                                         (mLabelVars     m)
    where
    p = 32 
    showi is = "[" ++ (intercalate "," $ map show is) ++ "]"
    showmv v = (pad p ' ' (mvarName v)) ++ ": " ++ showi (mvarIdx v)

----------------------------------------------------------
-- Private functions
----------------------------------------------------------

modelViews :: RModel c a b -> IO [View a b]
modelViews ref = (liftM mViews) $ readIORef ref
