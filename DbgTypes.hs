{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, ImplicitParams, FunctionalDependencies, UndecidableInstances, RecordWildCards #-}

module DbgTypes(Rel,
                Vals,
                View(..),
                ViewEvents(..),
                Type(..),
                State(..),
                isConcreteState,
                TranCategory(..),
                transitionCategory,
                Transition(..),
                tranTo',
                tranRel,
                isConcreteTransition,
                Oracle(..),
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
                mInitV,
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
                modelInitVars,
                modelUntrackedVars,
                modelStateRels,
                modelTransRels,
                modelConcretiseState,
                modelConcretiseTransition,
                modelActiveTransRel,
                modelAddOracle,
                modelAdviseTransition,
                modelSelectTransition,
                modelAddTransition,
                modelSelectState,
                modelSetConstraint,
                modelQuit,
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
data View a b d = View {
    viewName      :: String,
    viewDefAlign  :: IDEAlign,
    viewShow      :: IO (),
    viewHide      :: IO (),
    viewGetWidget :: IO G.Widget,
    viewQuit      :: IO Bool,      -- False = abandon quit
    viewCB        :: ViewEvents a b d
}

-- Events sent from the debugger core to each view
data ViewEvents a b d = ViewEvents {
     evtStateSelected      :: Maybe (State a d) -> IO (),
     evtTransitionSelected :: Transition a b d -> IO (),
     evtTRelUpdated        :: IO ()
}

data TranCategory = TranControllable
                  | TranUncontrollable
                  | TranNeutral

data State a d = State {
    sAbstract :: a,          -- abstract state
    sConcrete :: Maybe (d,a) -- concrete state consists of concrete variable assignment 
                             -- and abstract untracked var assignment
}

isConcreteState :: State a d -> Bool
isConcreteState = isJust . sConcrete


instance (?m::c, L.Boolean c a, Eq d) => Eq (State a d) where
    (==) x y = sAbstract x .== sAbstract y && (fmap fst $ sConcrete x) == (fmap fst $ sConcrete y)

transitionCategory :: (Rel c v a s, ?m::c) => Model c a b d -> Transition a b d -> IO TranCategory
transitionCategory model tran = do
    let c = sAbstract $ tranFrom tran
        l = tranAbstractLabel tran
        u = tranUntracked tran
        trel = c .& l .& u
    case find ((==contRelName) . fst) (mStateRels model) of
         Nothing        -> return TranNeutral
         Just (_, cont) -> if' (trel `leq` cont)    (return TranControllable) $
                           if' (trel `leq` nt cont) (return TranUncontrollable) $
                           return TranNeutral

data Transition a b d = Transition {
    tranFrom          :: State a d,
    tranUntracked     :: a,
    tranAbstractLabel :: a,
    tranConcreteLabel :: Maybe b,
    tranSrc           :: Maybe String,
    tranTo            :: State a d
}

instance (?m::c, L.Boolean c a, Eq b, Eq d) => Eq (Transition a b d) where
    (==) x y =  tranFrom x == tranFrom y
             && tranTo x == tranTo y
             && tranUntracked x .== tranUntracked y
             && tranAbstractLabel x .== tranAbstractLabel y
             && tranConcreteLabel x == tranConcreteLabel y

-- Project next state to current state vars
tranTo' :: (Rel c v a s, ?m::c) => Model c a b d -> Transition a b d -> a
tranTo' model tr = swap (mNextV model) (mStateV model) (sAbstract $ tranTo tr)

-- Conjunction of state, next, label, and untracked relations
tranRel :: (Rel c v a s, ?m::c) => Model c a b d -> Transition a b d -> a
tranRel model tr = (sAbstract $ tranFrom tr) 
                .& (tranTo' model tr)
                .& (tranUntracked tr)
                .& (tranAbstractLabel tr)

isConcreteTransition :: Transition a b d -> Bool
isConcreteTransition Transition{..} = isConcreteState tranFrom 
                                   && isConcreteState tranTo 
                                   && isJust tranConcreteLabel

-- Oracle interface for advising available transitions
data Oracle a b d = Oracle String (IO (Maybe (Transition a b d)))

data ModelVar = ModelVar { mvarName :: String
                         , mvarType :: Type
                         , mvarIdx  :: [Int]
                         }
instance Eq ModelVar where
    (==) v1 v2 = mvarName v1 == mvarName v2 && mvarIdx v1 == mvarIdx v2

type ModelStateVar = (String, Type, ([Int],[Int]))

-- Debugger state
data Model c a b d = Model {
    -- Static part --
    mCtx                  :: c,

    -- Abstract variable sections
    mStateVars            :: [ModelStateVar],
    mUntrackedVars        :: [ModelVar],
    mLabelVars            :: [ModelVar],
    mInitVars             :: [ModelVar],

    -- State and transition relations being debugged
    mStateRels            :: [(String, a)],
    mTransRels            :: [(String, a)],

    -- Callbacks
    mConcretiseState      :: a                -> Maybe (State a d),
    mConcretiseTransition :: Transition a b d -> Maybe (Transition a b d),

    -- Dynamic part --
    mViews                :: [View a b d],
    mAutoConcretiseTrans  :: Bool,
    mConstraints          :: M.Map String a,
    mTransRel             :: a,
    mOracles              :: [Oracle a b d]
}

mCurStateVars :: Model c a b d -> [ModelVar]
mCurStateVars = map (\(n,tp,(i,_)) -> ModelVar n tp i) . mStateVars

mNextStateVars :: Model c a b d -> [ModelVar]
mNextStateVars = map (\(n,tp,(_,i)) -> ModelVar n tp i) . mStateVars

mvarToVS :: (Rel c v a s, ?m::c) => ModelVar -> v
mvarToVS = idxToVS . mvarIdx

mStateV, mNextV, mUntrackedV, mLabelV, mInitV :: (Rel c v a s, ?m::c) => Model c a b d -> v
mStateV     = idxToVS . concatMap mvarIdx . mCurStateVars
mNextV      = idxToVS . concatMap mvarIdx . mNextStateVars
mUntrackedV = idxToVS . concatMap mvarIdx . mUntrackedVars
mLabelV     = idxToVS . concatMap mvarIdx . mLabelVars
mInitV      = idxToVS . concatMap mvarIdx . mInitVars

mSetConstraint :: (Rel c v a s) => Model c a b d -> String -> Maybe a -> Model c a b d
mSetConstraint m cname Nothing  = mUpdateTRel $ m {mConstraints = M.delete cname   $ mConstraints m}
mSetConstraint m cname (Just r) = mUpdateTRel $ m {mConstraints = M.insert cname r $ mConstraints m}

mUpdateTRel :: (Rel c v a s) => Model c a b d -> Model c a b d
mUpdateTRel m@Model{..} = m {mTransRel = r}
    where r = let ?m = mCtx in 
              conj $ (snd $ head mTransRels) : (map snd $ M.toList mConstraints)

mAddOracle :: Model c a b d -> Oracle a b d -> Model c a b d
mAddOracle m oracle = m {mOracles = mOracles m ++ [oracle]}

mAdviseTransition :: Model c a b d -> IO (Maybe (Transition a b d))
mAdviseTransition m = mAdviseTransition' (mOracles m)

mAdviseTransition' :: [Oracle a b d] -> IO (Maybe (Transition a b d))
mAdviseTransition' []                = return Nothing
mAdviseTransition' ((Oracle _ f):os) = f >>= maybe (mAdviseTransition' os) (return . Just)

type RModel c a b d = IORef (Model c a b d)

----------------------------------------------------------
-- External interface
----------------------------------------------------------

-- Querying state
modelCtx :: RModel c a b d -> IO c
modelCtx ref = (liftM mCtx) $ readIORef ref

modelStateVars :: RModel c a b d -> IO [(String, Type, ([Int],[Int]))]
modelStateVars ref = getIORef mStateVars ref

modelUntrackedVars :: RModel c a b d -> IO [ModelVar]
modelUntrackedVars ref = getIORef mUntrackedVars ref

modelLabelVars :: RModel c a b d -> IO [ModelVar]
modelLabelVars ref = getIORef mLabelVars ref

modelInitVars :: RModel c a b d -> IO [ModelVar]
modelInitVars ref = getIORef mInitVars ref

modelTransRels :: RModel c a b d -> IO [(String, a)]
modelTransRels ref = getIORef mTransRels ref

modelStateRels :: RModel c a b d -> IO [(String, a)]
modelStateRels ref = getIORef mStateRels ref

modelConcretiseState :: RModel c a b d -> a -> IO (Maybe (State a d))
modelConcretiseState ref x = getIORef ((flip mConcretiseState) x) ref

modelConcretiseTransition :: RModel c a b d -> Transition a b d -> IO (Maybe (Transition a b d))
modelConcretiseTransition ref tran = do
    Model{..} <- readIORef ref
    return $ if isConcreteTransition tran 
                then Just tran
                else mConcretiseTransition tran

modelActiveTransRel :: RModel c a b d -> IO a
modelActiveTransRel ref = getIORef mTransRel ref

modelAddOracle :: RModel c a b d -> Oracle a b d -> IO ()
modelAddOracle ref oracle = modifyIORef ref $ \m -> mAddOracle m oracle

modelAdviseTransition :: RModel c a b d -> IO (Maybe (Transition a b d))
modelAdviseTransition ref = readIORef ref >>= mAdviseTransition

-- Actions
modelSelectTransition :: RModel c a b d -> Transition a b d -> IO ()
modelSelectTransition ref tran = doSelectTransition ref tran False

modelAddTransition :: RModel c a b d -> Transition a b d -> IO ()
modelAddTransition ref tran = doSelectTransition ref tran True

doSelectTransition :: RModel c a b d -> Transition a b d -> Bool -> IO ()
doSelectTransition ref tran selnext = do
   Model{..} <- readIORef ref
   let tran' = if (not $ isConcreteTransition tran) && mAutoConcretiseTrans
                  then case mConcretiseTransition tran of 
                            Nothing -> tran
                            Just tr -> tr
                  else tran
   _ <- mapM (\v -> (evtTransitionSelected $ viewCB v) tran') mViews
   when selnext (do {_ <- mapM (\v -> (evtStateSelected $ viewCB v) (Just $ tranTo tran')) mViews; return ()})
   return ()



modelSelectState :: RModel c a b d -> Maybe (State a d) -> IO ()
modelSelectState ref mrel = do
   views <- modelViews ref
   _ <- mapM (\v -> (evtStateSelected $ viewCB v) mrel) views
   return ()

modelSetConstraint :: (Rel c v a s) => RModel c a b d -> String -> Maybe a -> IO ()
modelSetConstraint ref cname crel = do 
    modifyIORef ref $ \m -> mSetConstraint m cname crel
    views <- modelViews ref
    _ <- mapM (evtTRelUpdated . viewCB) views
    return ()

modelQuit :: RModel c a b d -> IO Bool
modelQuit ref = do
    m <- readIORef ref
    modelQuit' $ mViews m

modelQuit' :: [View a b d] -> IO Bool
modelQuit' []     = return True
modelQuit' (v:vs) = do q <- viewQuit v
                       if' q (modelQuit' vs) (return False)

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
    return $ map (\v -> (v, boolArrToBitsBe $ extract (mvarToVS v) asn)) $ nub supvars

-- Message boxes
showMessage :: RModel c a b d -> G.MessageType -> String -> IO ()
showMessage _ mtype mtext = do
    dialog <- G.messageDialogNew Nothing [G.DialogModal] mtype G.ButtonsOk mtext
    _ <- G.onResponse dialog (\_ -> G.widgetDestroy dialog)
    G.windowPresent dialog

----------------------------------------------------------
-- Debugging
----------------------------------------------------------

mDumpIndices :: Model c a b d -> String
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

modelViews :: RModel c a b d -> IO [View a b d]
modelViews ref = (liftM mViews) $ readIORef ref
