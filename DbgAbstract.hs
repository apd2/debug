{-# LANGUAGE ImplicitParams, RecordWildCards #-}

module DbgAbstract (abstractState,
                    abstractUntracked,
                    abstractLabel,
                    abstractTransition) where

import qualified Data.Map as M

import Util
import qualified DbgTypes as D
import IExpr
import ISpec
import IVar
import Predicate
import Store
import qualified Implicit as Imp

abstractRel :: (D.Rel c v a s, ?spec::Spec, ?m::c, ?absvars::M.Map String AbsVar) => [D.ModelVar] -> Store -> a
abstractRel vars store = Imp.conj $ map (evalAbsVar store) vars

abstractState :: (D.Rel c v a s, ?spec::Spec, ?m::c, ?model::D.Model c a Store, ?absvars::M.Map String AbsVar) => Store -> D.State a Store
abstractState store = D.State { D.sAbstract = abstractRel (D.mCurStateVars ?model) store
                              , D.sConcrete = Just $ storeProject store $ map varName $ specStateVar ?spec
                              }

abstractUntracked :: (D.Rel c v a s, ?spec::Spec, ?m::c, ?model::D.Model c a Store, ?absvars::M.Map String AbsVar) => Store -> a
abstractUntracked = abstractRel (D.mUntrackedVars ?model)

abstractLabel :: (D.Rel c v a s, ?spec::Spec, ?m::c, ?model::D.Model c a Store, ?absvars::M.Map String AbsVar) => Store -> a
abstractLabel = abstractRel (D.mLabelVars ?model)

abstractTransition :: (D.Rel c v a s, ?spec::Spec, ?m::c, ?model::D.Model c a Store, ?absvars::M.Map String AbsVar) => D.State a Store -> Store -> D.Transition a Store
abstractTransition from to = D.Transition {
        tranFrom = from,
        -- compute untracked and label predicates over to
        tranUntracked     = abstractUntracked $ fromJustMsg "abstractTransition: no concrete state" $ D.sConcrete from,
        tranAbstractLabel = abstractLabel     to,
        -- project "to" state on "tmp" variables
        tranConcreteLabel = Just $ storeProject to (map varName $ specTmpVar ?spec),
        tranTo            = abstractState to
    }


evalAbsVar :: (D.Rel c v a s, ?spec::Spec, ?m::c, ?absvars::M.Map String AbsVar) => Store -> D.ModelVar -> a
evalAbsVar store D.ModelVar{..} = evalAbsVar' store (?absvars M.! mvarName) mvarIdx

evalAbsVar' :: (D.Rel c v a s, ?spec::Spec, ?m::c) => Store -> AbsVar -> [Int] -> a
evalAbsVar' store (AVarPred p) is = 
    if storeEvalBool store (predToExpr p)
       then Imp.eqConst (D.idxToVS is) (1::Int)
       else Imp.eqConst (D.idxToVS is) (0::Int)

evalAbsVar' store (AVarTerm term) is = 
   Imp.eqConst (D.idxToVS is) $ scalarToInt $ storeEvalScalar store (termToExpr term)

--evalAbsVar' store (AVarEnum name vals) is =
--    case findIndex (==v) vals of
--         Just i  -> eqConst vs i
--         Nothing -> conj $ map (nt $ eqConst vs i) [0..length vals -1]
--    where v  = storeEvalScalar store (I.EVar name)
--          vs = D.idxToVS is

scalarToInt :: (?spec::Spec) => Val -> Integer
scalarToInt (BoolVal True)  = 1
scalarToInt (BoolVal False) = 0
scalarToInt (UIntVal _ i)   = i
scalarToInt (SIntVal _ i)   = i
scalarToInt (EnumVal   s)   = fromIntegral $ enumToInt s
