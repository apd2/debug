module DbgConcretise (concretiseState,
                      concretiseLabel) where

import qualified Data.Map as M

import Store
import SMTSolver

-- Input: relation describing a set of abstract states
-- Output: a single concrete state or Nothing if a 
-- satisfying assignment could not be found
concretiseState :: SMTSolver -> M.Map String AbsVar -> a -> Maybe (D.State a Store)
concretiseState solver absvars rel = do
    -- Find one satisfying assignment of rel (return Nothing
    -- if one does not exist)
    let cube = oneCube rel
    asn <- oneSatVal cube (mCurStateVars model)
    let preds = map (\(mvar, val) -> I.varAsnToPred (absvars M.! mvarName mvar) val) asn
    -- Try to concretise this assignment
    case smtGetModel solver $ map FPred preds of
         Nothing            -> Nothing
         Just (Left core)   -> do -- Remove unsat core from rel and repeat
                let unsatcube = conj
                                $ map (\(mvar, v) -> eqConst (D.idxToVS (mvarIdx mvar)) v) 
                                $ map (asn !!) core
                    rel' = rel .& (nt unsatcube)
                concretiseState absvars rel'
         Just (Right store) -> D.State {sAbstract = cube, sConcrete = store}

-- Given a concrete state and an abstract label, compute 
-- concrete label.  
-- The abstract label is assumed to be a cube.
concretiseLabel :: SMTSolver -> M.Map String AbsVar -> Store -> a -> Maybe Store
concretiseLabel solver absvars cstate alabel = do
   -- extract predicates from abstract label
   asn <- oneSatVal alabel (mCurStateVars model ++ mLabelVars model)
   let lpreds = map (\(mvar, val) -> I.varAsnToPred (absvars M.! mvarName mvar) val) asn
       -- extract values of relevant state variables from concrete 
       -- state and transform them into additional predicates
       spreds = map (\term -> PAtom REq term $ (valToTerm $ storeEvalScalar cstate $ termToExpr term))
                $ nub 
                $ filter ((== VarState) . termCategory) 
                $ concatMap predTerm preds
   -- Check for model
   case smtGetModel solver $ map FPred $ lpreds ++ spreds of
        Just (Right (SStruct fs)) -> -- Keep temporary variables only
                                     Just $ SStruct $ M.filterWithKey (\n _ -> (varCat $ getVar n) == VarTmp) fs 
        _                         -> Nothing
