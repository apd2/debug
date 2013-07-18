{-# LANGUAGE ImplicitParams, RecordWildCards #-}

-- Concretising relations over abstract variables

module DbgConcretise (concretiseRel,
                      concretiseState,
                      concretiseLabel,
                      concretiseTransition) where

                      
import Data.List
import qualified Data.Map    as M
import Debug.Trace

import Store
import SMTSolver
import Predicate
import qualified DbgTypes    as D
import qualified SourceView  as D
import Implicit
import BFormula
import qualified Spec        as F

import IVar
import ISpec

-- Input: relation over a set of abstract variables
-- Output: a single concrete assignment or Nothing if a 
-- satisfying assignment could not be found
concretiseRel :: (D.Rel c v a s, ?spec::Spec, ?m::c, ?solver::SMTSolver, ?absvars::M.Map String AbsVar) => [D.ModelVar] -> a -> Maybe (a, Store)
concretiseRel mvars0 rel = do
    let mvars = nub mvars0
    -- Find one satisfying assignment of rel (return Nothing
    -- if one does not exist)
    qb <- oneCube (D.idxToVS $ concatMap D.mvarIdx mvars) rel
    asn <- D.oneSatVal qb mvars
    let preds = map (\(mvar, val) -> avarAsnToPred (?absvars M.! D.mvarName mvar) val) asn
    -- Try to concretise this assignment
    case smtGetModel ?solver $ map FPred preds of
         Nothing            -> Nothing
         Just (Left core)   -> do -- Remove unsat core from rel and repeat
                let unsatcube = trace ("concretiseRel (" ++ (show $ length mvars) ++ " vars): core = " ++ show core)
                                $ conj
                                $ map (\(mvar, v) -> eqConst (D.idxToVS (D.mvarIdx mvar)) v) 
                                $ map (asn !!) core
                    rel' = rel .& (nt unsatcube)
                concretiseRel mvars rel'
         Just (Right store) -> return (qb, store)

concretiseState :: (D.Rel c v a s, ?spec::Spec, ?m::c, ?solver::SMTSolver, ?model::D.Model c a Store, ?absvars::M.Map String AbsVar) => a -> Maybe (D.State a Store)
concretiseState rel = case concretiseRel (D.mCurStateVars ?model ++ D.mInitVars ?model) rel of
                           Nothing            -> Nothing
                           Just (rel', store) -> Just $ D.State rel' (Just $ storeExtendDefaultState store)

-- Given a concrete state and an abstract label, compute concrete label.  
-- The abstract label is assumed to be a cube.
concretiseLabel :: (D.Rel c v a s, ?spec::Spec, ?m::c, ?solver::SMTSolver, ?model::D.Model c a Store, ?absvars::M.Map String AbsVar) => Store -> a -> Maybe Store
concretiseLabel cstate alabel = do
   -- extract predicates from abstract label
   asn <- D.oneSatVal alabel (D.mCurStateVars ?model ++ D.mLabelVars ?model)
   let lpreds = map (\(mvar, val) -> avarAsnToPred (?absvars M.! D.mvarName mvar) val) 
                $ filter (not . D.isEnVarName . D.mvarName . fst) asn
       -- extract values of relevant state variables from concrete 
       -- state and transform them into additional predicates
       spreds = map (\term -> PAtom REq term $ (valToTerm $ storeEvalScalar cstate $ termToExpr term))
                $ nub 
                $ filter ((== VarState) . termCategory) 
                $ concatMap predTerm lpreds
   -- Check for model
   case smtGetModel ?solver $ map FPred $ lpreds ++ spreds of
        Just (Right (SStruct fs)) -> -- Keep temporary variables only
                                     Just $ storeExtendDefaultLabel $ SStruct $ M.filterWithKey (\n _ -> (varCat $ getVar n) == VarTmp) fs
        _                         -> Nothing

-- Inputs:
-- * Concrete from-state
-- * Abstract label
-- * Abstract next-state
--
-- Outputs: 
-- * Concrete next-state and label store, which
--   can be used to compute other components of 
--   the transition
-- * String that represents controllable action 
--   performed by the transition in a user-readable
--   way
concretiseTransition :: (D.Rel c v a s, ?flatspec::F.Spec, ?spec::Spec, ?m::c, ?solver::SMTSolver, ?model::D.Model c a Store, ?absvars::M.Map String AbsVar) => Store -> a -> Maybe Store
concretiseTransition cstate alabel = do
    -- concretise label
    clabel <- concretiseLabel cstate alabel
    cnstore <- D.simulateTransition ?flatspec ?spec ?absvars cstate clabel
    return cnstore
