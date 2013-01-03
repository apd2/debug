{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, ImplicitParams, FunctionalDependencies #-}

module DbgTypes(Rel,
                View(..),
                ViewEvents(..),
                Type(..),
                Transition(..),
                tranTo,
                Model(..),
                RModel,
                mStateV, 
                mNextV, 
                mUntrackedV, 
                mLabelV,
                contRelName,
                idxToVS,
                valStrFromInt,
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
       Eq a, 
       Show a) => Rel c v a s | c -> v, c -> a, c -> s

idxToVS :: (Rel c v a s, ?m::c) => [Int] -> v
idxToVS indices = vconcat $ map varAtIndex indices

-- Debugger's own type system
data Type = Bool
          | SInt Int
          | UInt Int
          | Enum [String]

instance Show Type where
    show Bool      = "bool"
    show (SInt i)  = "sint<" ++ show i ++ ">"
    show (UInt i)  = "uint<" ++ show i ++ ">"
    show (Enum es) = "enum"

valStrFromInt :: Type -> Integer -> String
valStrFromInt Bool      0                                = "false"
valStrFromInt Bool      1                                = "true"
valStrFromInt (Enum es) i | length es >= fromInteger i+1 = es !! (fromInteger i)
                          | otherwise                    = "?"
valStrFromInt _         i                                = show i

-- View interface
data View a = View {
    viewName      :: String,
    viewDefAlign  :: IDEAlign,
    viewShow      :: IO (),
    viewHide      :: IO (),
    viewGetWidget :: IO G.Widget,
    viewCB        :: ViewEvents a
}

-- Events sent from the debugger core to each view
data ViewEvents a = ViewEvents {
     evtStateSelected      :: Maybe a -> IO (),
     evtTransitionSelected :: Transition a -> IO ()
}

data Transition a = Transition {
    tranFrom      :: a,
    tranUntracked :: a,
    tranLabel     :: a,
    tranTo'       :: a
}

-- Project next state to current state vars
tranTo :: (Rel c v a s, ?m::c) => Model c a -> Transition a -> a
tranTo model t = trace ("tranTo: " ++ show (supportIndices $ tranTo' t) ++ "->" ++ show (supportIndices res)) res
    where res = swap (mNextV model) (mStateV model) (tranTo' t)


-- Debugger state
data Model c a = Model {
    mCtx           :: c,

    -- Variable sections
    mStateVars     :: [(String, Type, ([Int],[Int]))],
    mUntrackedVars :: [(String, Type, [Int])],
    mLabelVars     :: [(String, Type, [Int])],

    -- State and transition relations being debugged
    mStateRels     :: [(String, a)],
    mTransRels     :: [(String, a)],

    mViews         :: [View a]
}

mStateV, mNextV, mUntrackedV, mLabelV :: (Rel c v a s, ?m::c) => Model c a -> v
mStateV     = vconcat . map varAtIndex . concatMap (fst . trd3) . mStateVars
mNextV      = vconcat . map varAtIndex . concatMap (snd . trd3) . mStateVars
mUntrackedV = vconcat . map varAtIndex . concatMap trd3         . mUntrackedVars
mLabelV     = vconcat . map varAtIndex . concatMap trd3         . mLabelVars


type RModel c a = IORef (Model c a)


----------------------------------------------------------
-- External interface
----------------------------------------------------------

-- Querying state
modelCtx :: RModel c a -> IO c
modelCtx ref = (liftM mCtx) $ readIORef ref

modelStateVars :: RModel c a -> IO [(String, Type, ([Int],[Int]))]
modelStateVars ref = (liftM mStateVars) $ readIORef ref

modelUntrackedVars :: RModel c a -> IO [(String, Type, [Int])]
modelUntrackedVars ref = (liftM mUntrackedVars) $ readIORef ref

modelLabelVars :: RModel c a -> IO [(String, Type, [Int])]
modelLabelVars ref = (liftM mLabelVars) $ readIORef ref

modelTransRels :: RModel c a -> IO [(String, a)]
modelTransRels ref = (liftM mTransRels) $ readIORef ref

modelStateRels :: RModel c a -> IO [(String, a)]
modelStateRels ref = (liftM mStateRels) $ readIORef ref

-- TODO: implement proper selection of transition relation to debug
modelActiveTransRel :: RModel c a -> IO a
modelActiveTransRel ref = (liftM (snd . head . mTransRels)) $ readIORef ref

-- Actions
modelSelectTransition :: RModel c a -> Transition a -> IO ()
modelSelectTransition ref tran = do
   views <- modelViews ref
   mapM (\v -> (evtTransitionSelected $ viewCB v) tran) views
   return ()

modelSelectState :: RModel c a -> Maybe a -> IO ()
modelSelectState ref mrel = do
   views <- modelViews ref
   mapM (\v -> (evtStateSelected $ viewCB v) mrel) views
   return ()

----------------------------------------------------------
-- Private functions
----------------------------------------------------------

modelViews :: RModel c a -> IO [View a]
modelViews ref = (liftM mViews) $ readIORef ref
