{-# LANGUAGE ImplicitParams, RecordWildCards #-}

-- Concretising relations over abstract variables

module DbgConcretise (concretiseRel,
                      concretiseLabel,
                      concretiseTransition) where

import Data.List
import qualified Data.Map as M

import Store
import SMTSolver
import Predicate
import qualified DbgTypes as D
import Implicit
import BFormula
import IVar
import ISpec
import DbgAbstract

-- Input: relation over a set of abstract variables
-- Output: a single concrete assignment or Nothing if a 
-- satisfying assignment could not be found
concretiseRel :: (D.Rel c v a s, ?spec::Spec, ?m::c, ?solver::SMTSolver, ?absvars::M.Map String AbsVar) => [D.ModelVar] -> a -> Maybe (a, Store)
concretiseRel mvars rel = do
    -- Find one satisfying assignment of rel (return Nothing
    -- if one does not exist)
    qb <- oneCube (D.idxToVS $ concatMap D.mvarIdx mvars) rel
    asn <- D.oneSatVal qb mvars
    let preds = map (\(mvar, val) -> avarAsnToPred (?absvars M.! D.mvarName mvar) val) asn
    -- Try to concretise this assignment
    case smtGetModel ?solver $ map FPred preds of
         Nothing            -> Nothing
         Just (Left core)   -> do -- Remove unsat core from rel and repeat
                let unsatcube = conj
                                $ map (\(mvar, v) -> eqConst (D.idxToVS (D.mvarIdx mvar)) v) 
                                $ map (asn !!) core
                    rel' = rel .& (nt unsatcube)
                concretiseRel mvars rel'
         Just (Right store) -> return (qb, store)

-- Given a concrete state and an abstract label, compute concrete label.  
-- The abstract label is assumed to be a cube.
concretiseLabel :: (D.Rel c v a s, ?spec::Spec, ?m::c, ?solver::SMTSolver, ?model::D.Model c a Store, ?absvars::M.Map String AbsVar) => Store -> a -> Maybe Store
concretiseLabel cstate alabel = do
   -- extract predicates from abstract label
   asn <- D.oneSatVal alabel (D.mCurStateVars ?model ++ D.mLabelVars ?model)
   let lpreds = map (\(mvar, val) -> avarAsnToPred (?absvars M.! D.mvarName mvar) val) asn
       -- extract values of relevant state variables from concrete 
       -- state and transform them into additional predicates
       spreds = map (\term -> PAtom REq term $ (valToTerm $ storeEvalScalar cstate $ termToExpr term))
                $ nub 
                $ filter ((== VarState) . termCategory) 
                $ concatMap predTerm lpreds
   -- Check for model
   case smtGetModel ?solver $ map FPred $ lpreds ++ spreds of
        Just (Right (SStruct fs)) -> -- Keep temporary variables only
                                     Just $ SStruct $ M.filterWithKey (\n _ -> (varCat $ getVar n) == VarTmp) fs 
        _                         -> Nothing

concretiseTransition :: (D.Rel c v a s, ?spec::Spec, ?m::c, ?solver::SMTSolver, ?model::D.Model c a Store, ?absvars::M.Map String AbsVar) => Store -> a -> Maybe (D.Transition a Store)
concretiseTransition cstate alabel = do
    let tranFrom          = abstractState     cstate
        tranUntracked     = abstractUntracked cstate
        tranAbstractLabel = alabel
    clabel <- concretiseLabel cstate alabel
    let cto = simulateTransition cstate clabel
        tranConcreteLabel = Just $ storeProject cto $ map varName $ specTmpVar ?spec
        tranTo = abstractState cto
    return $ D.Transition{..}


simulateTransition = error "simulateTransition not implemented"
