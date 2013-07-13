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
import qualified Method      as F
import qualified NS          as F
import qualified Expr        as F
import qualified InstTree    as F
import qualified Name        as F
import qualified Type        as F
import qualified TypeOps     as F
import qualified Val         as F
import qualified Pos         as F
import qualified ExprFlatten as F
import qualified TypeOps     as F
import PID
import IVar
import ISpec
import Inline
import IExpr hiding (conj)

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
                let unsatcube = trace ("concretiseRel (" ++ (show $ length mvars) ++ " vars): core = " ++ show core)
                                $ conj
                                $ map (\(mvar, v) -> eqConst (D.idxToVS (D.mvarIdx mvar)) v) 
                                $ map (asn !!) core
                    rel' = rel .& (nt unsatcube)
                concretiseRel mvars rel'
         Just (Right store) -> return (qb, store)

concretiseState :: (D.Rel c v a s, ?spec::Spec, ?m::c, ?solver::SMTSolver, ?model::D.Model c a Store, ?absvars::M.Map String AbsVar) => a -> Maybe (D.State a Store)
concretiseState rel = case concretiseRel (D.mCurStateVars ?model) rel of
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
concretiseTransition :: (D.Rel c v a s, ?flatspec::F.Spec, ?spec::Spec, ?m::c, ?solver::SMTSolver, ?model::D.Model c a Store, ?absvars::M.Map String AbsVar) => Store -> a -> Maybe Store
concretiseTransition cstate alabel = do
    -- concretise label
    clabel <- concretiseLabel cstate alabel
    cnstore <- D.simulateTransition ?flatspec ?spec ?absvars cstate clabel
    return cnstore


data ContAction = ActExit  -- exit magic block
                | ActCall {caTask :: F.Method, caArgs :: [(F.Arg, F.Expr)]}

-- Translate an abstract transition into a controllable action
-- sc     - scope in the original input spec in which the transition is to be executed (i.e., the one which contains magic block)
-- cstate - concrete state before the transition
-- alabel - abstract transition label
transitionToAction :: (?flatspec::F.Spec, ?inspec::F.Spec, ?spec::Spec, ?solver::SMTSolver, ?model::D.Model c a Store, ?absvars::M.Map String AbsVar, D.Rel c v a s, ?m::c) => F.Scope -> Store -> a -> Maybe ContAction
transitionToAction sc cstate alabel = do
    cnstate <- concretiseTransition cstate alabel
    let tag = storeEvalEnum cnstate mkTagVar
        sciid = case sc of
                     F.ScopeMethod  _ m -> fst $ F.itreeParseName (F.name m)
                     F.ScopeProcess _ p -> fst $ F.itreeParseName (F.name p)
    if tag == mkTagIdle
       then if not $ storeEvalBool cnstate mkMagicVar
               then return ActExit
               else Nothing
       else do let (methiid, methname) = F.itreeParseName (F.Ident F.nopos tag)
               let ?spec = ?inspec
               path <- F.itreeAbsToRelPath sciid methiid
               let (tm, caTask) = F.getMethod sc (F.MethodRef F.nopos (path++[methname]))
                   caArgs = map (\a -> (a, let ?scope = F.ScopeTemplate tm in storeToFExpr a sc
                                           $ storeEval cnstate 
                                           $ (EVar $ mkVarNameS (NSID Nothing (Just caTask)) $ F.sname a)))
                            $ filter ((== F.ArgIn) . F.argDir)
                            $ F.methArg caTask
               return ActCall{..}


storeToFExpr :: (?spec::F.Spec, F.WithType a) => a -> F.Scope -> Store -> F.Expr
storeToFExpr x sc s = 
    let F.Type xsc  t  = F.typ x
        F.Type xsc' t' = F.typ' x
    in case t' of
            F.StructSpec _ fs  -> F.EStruct F.nopos tname (Left $ map (\f -> (F.name f, let ?scope = xsc' in storeToFExpr f sc (sfs M.! F.sname f))) fs)
                                  where F.UserTypeSpec _ tname = t
                                        tname' = F.staticSymReScope (tname, xsc) sc
                                        SStruct sfs = s
            F.ArraySpec  _ t _ -> error "storeToFExpr does not support array arguments"
            _                  -> valToFExpr (F.typ' x) v
                                  where SVal v = s

valToFExpr :: F.Type -> Val -> F.Expr
valToFExpr (F.Type _   (F.BoolSpec _))    (BoolVal True)  = F.EBool F.nopos True
valToFExpr (F.Type _   (F.BoolSpec _))    (BoolVal False) = F.EBool F.nopos False
valToFExpr (F.Type _   (F.SIntSpec _ w))  (SIntVal _ i)   = F.ELit  F.nopos w True  F.Rad10 i
valToFExpr (F.Type _   (F.UIntSpec _ w))  (UIntVal _ i)   = F.ELit  F.nopos w False F.Rad10 i
valToFExpr (F.Type sc' (F.EnumSpec _ es)) (EnumVal e)     = F.ETerm F.nopos (F.unflattenName $ F.Ident F.nopos e)
valToFExpr t                              _               = error $ "valToFExpr: type " ++ (show $ F.tspec t) ++ " not supported"
