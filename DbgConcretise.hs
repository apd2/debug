{-# LANGUAGE ImplicitParams, RecordWildCards #-}

-- Concretising relations over abstract variables

module DbgConcretise (concretiseRel,
                      concretiseState,
                      concretiseLabel,
                      concretiseTransition) where

                      
import Data.List
import Data.Maybe
import qualified Data.Map    as M
import Debug.Trace

import Util hiding (trace)
import Store
import SMTSolver
import Predicate
import qualified DbgTypes        as D
import qualified SourceView      as D
import qualified SourceViewTypes as D
import Implicit
import BFormula
import qualified Spec        as F

import IVar
import ISpec
import IExpr hiding (conj)

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
    let fs = map (\(mvar, val) -> let av = ?absvars M.! D.mvarName mvar in 
                                  avarAsnToFormula av val) asn
    -- Try to concretise this assignment
    case smtGetModelOrCore ?solver fs of
         Nothing            -> Nothing
         Just (Left core)   -> do -- Remove unsat core from rel and repeat
                let unsatcube = trace ("concretiseRel (" ++ (show $ length mvars) ++ " vars): core = " ++ show core)
                                $ conj
                                $ map (\(mvar, v) -> eqConst (D.idxToVS (D.mvarIdx mvar)) v) 
                                $ map (asn !!) core
                    rel' = rel .& (nt unsatcube)
                concretiseRel mvars rel'
         Just (Right store) -> return (qb, store)

concretiseState :: (D.Rel c v a s, ?spec::Spec, ?m::c, ?solver::SMTSolver, ?model::D.Model c a Store D.SVStore, ?absvars::M.Map String AbsVar) => a -> Maybe (D.State a D.SVStore)
concretiseState rel = case concretiseRel (D.mCurStateVars ?model ++ D.mInitVars ?model ++ D.mUntrackedVars ?model) rel of
                           Nothing            -> Nothing
                           Just (rel', store) -> do st  <- oneCube (D.mStateV ?model)     rel'
                                                    unt <- oneCube (D.mUntrackedV ?model) rel'
                                                    return $ D.State st (Just $ (D.SVStore (storeExtendDefaultState store) [], unt))

-- Given a concrete state and an abstract label, compute concrete label.  
-- The abstract label is assumed to be a cube.
concretiseLabel :: (D.Rel c v a s, ?spec::Spec, ?m::c, ?solver::SMTSolver, ?model::D.Model c a Store D.SVStore, ?absvars::M.Map String AbsVar) => Store -> a -> Maybe Store
concretiseLabel cstate alabel = do
   -- extract predicates from abstract label
   asn <- D.oneSatVal alabel (D.mCurStateVars ?model ++ D.mLabelVars ?model)
   let lfs = mapMaybe (\(mvar, val) -> case find ((==(D.mkEnVarName $ D.mvarName mvar)) . D.mvarName . fst) asn of
                                            Nothing            -> Just $ avarAsnToFormula (?absvars M.! D.mvarName mvar) val 
                                            Just (_, val') -> if val' == 0
                                                                 then Nothing
                                                                 else Just $ avarAsnToFormula (?absvars M.! D.mvarName mvar) val)
             $ filter (not . D.isEnVarName . D.mvarName . fst) asn
       -- extract values of relevant state variables from concrete 
       -- state and transform them into additional predicates
       sfs = map (\term -> fRel REq (termToExpr term) $ (EConst $ storeEvalScalar cstate $ termToExpr term))
             $ nub 
             $ filter ((== VarState) . termCategory) 
             $ concatMap (concatMap avarTerms . fAbsVars) lfs
   -- Check for model
   case smtGetModel ?solver $ lfs ++ sfs of
        Just (Just (SStruct fs sp)) -> -- Keep temporary variables only
                                       Just $ storeExtendDefaultLabel $ SStruct (M.filterWithKey (\n _ -> (varCat $ getVar n) == VarTmp) fs) sp
        _                           -> Nothing

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
concretiseTransition :: (D.Rel c v a s, ?flatspec::F.Spec, ?spec::Spec, ?m::c, ?solver::SMTSolver, ?model::D.Model c a Store D.SVStore, ?absvars::M.Map String AbsVar) => Store -> a -> Maybe Store
concretiseTransition cstate alabel = do
    -- concretise label
    clabel <- concretiseLabel cstate alabel
    cnstore <- D.simulateTransition ?flatspec ?spec ?absvars cstate clabel
    return cnstore
