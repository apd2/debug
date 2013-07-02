{-# LANGUAGE RecordWildCards, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances, UndecidableInstances, ImplicitParams, TupleSections #-}

-- Interface to the abstraction library

module AbstractorIFace( SynthesisRes(..)
                      , mkSynthesisRes
                      , mkModel) where

import qualified Data.Map as M
import Data.List
import Data.Maybe

import Util
import Cudd
import CuddConvert
import Interface
import TermiteGame
import Implicit
import CuddSymTab
import Predicate
import Store
import SMTSolver
import qualified ISpec          as I
import qualified TranSpec       as I
import qualified IType          as I
import qualified DbgTypes       as D
import qualified DbgConcretise  as D
import qualified DbgAbstract    as D

instance D.Rel DdManager VarData DdNode [[SatBit]]

data SynthesisRes c a = SynthesisRes { srWin           :: Bool
                                     , srCtx           :: c
                                     , srStateVars     :: [D.ModelStateVar]
                                     , srUntrackedVars :: [D.ModelVar]
                                     , srLabelVars     :: [D.ModelVar]
                                     , srAbsVars       :: M.Map String AbsVar
                                     , srCont          :: a
                                     , srInit          :: a
                                     , srGoals         :: [a]
                                     , srFairs         :: [a]
                                     , srTran          :: a
                                     , srCPlusC        :: a
                                     , srCMinusC       :: a
                                     , srCPlusU        :: a
                                     , srCMinusU       :: a
                                     }

mkSynthesisRes :: I.Spec -> STDdManager s u -> ((Bool, RefineStatic s u, RefineDynamic s u), DB s u AbsVar AbsVar) -> SynthesisRes DdManager DdNode
mkSynthesisRes spec m ((res, RefineStatic{..}, RefineDynamic{..}), pdb) = 
    let ?spec = spec in
    let SectionInfo{..} = _sections pdb
        SymbolInfo{..}  = _symbolTable pdb
        (state, untracked) = partition func $ M.toList _stateVars
            where func (_, (_, is, _, _)) = not $ null $ intersect is _trackedInds

        srWin           = res 
        srCtx           = toDdManager m
        srStateVars     = map toTupleState state
            where toTupleState        (p, (_, is, _, is')) = (show p, avarType p, (is, is'))
        srUntrackedVars = map toModelVarUntracked untracked
            where toModelVarUntracked (p, (_, is, _, _)) = D.ModelVar (show p) (avarType p) is
        srLabelVars     = concatMap toModelVarLabel $ M.toList _labelVars
            where toModelVarLabel     (p, (_, is, _, ie)) = [D.ModelVar (show p) (avarType p) is, D.ModelVar (show p ++ ".en") D.Bool [ie]]
        srAbsVars       = M.fromList $ map (\v -> (show v,v)) $ (M.keys _stateVars) ++ (M.keys _labelVars)
        srCont          = toDdNode srCtx cont
        srInit          = toDdNode srCtx init
        srGoals         = map (toDdNode srCtx) goal
        srFairs         = map (toDdNode srCtx) fair
        srTran          = toDdNode srCtx trans
        srCMinusC       = toDdNode srCtx consistentMinusCULCont
        srCPlusC        = toDdNode srCtx consistentPlusCULCont
        srCMinusU       = toDdNode srCtx consistentMinusCULUCont
        srCPlusU        = toDdNode srCtx consistentPlusCULUCont
    in SynthesisRes{..}

-- Extract type information from AbsVar
avarType :: (?spec::I.Spec) => AbsVar -> D.Type
avarType (AVarPred _)  = D.Bool
avarType (AVarTerm tr) = case I.typ tr of
                              I.Bool   -> D.Bool
                              I.Enum n -> D.Enum $ (I.enumEnums $ I.getEnumeration n)
                              I.SInt w -> D.SInt w
                              I.UInt w -> D.UInt w

-- Construct transition relation that represent the result of three-valued 
-- abstraction.
--
-- If srWin == True then
--   T := quantify_dis (Tc /\ c-c) \/ (Tu /\ c+u)
-- else
--   T := (Tc /\ c+c) \/ quantify_dis (Tc /\ c-u)
--
-- where Tc and Tu are controllable and uncontrollable transition relations,
-- and the quantify_dis function quantifies away disabled variables.  For a 
-- single variable X:
--
-- quantify_dis_X(rel) = (X.en /\ rel) \/ (!X.en /\ exists X. rel)
-- 
mkTRel :: (D.Rel c v a s) => SynthesisRes c a -> a
mkTRel sr@SynthesisRes{..} =
    let ?m     = srCtx in
    let tcont  = srTran .& srCont
        tucont = srTran .& (nt srCont)
    in trace "mkTRel"
       $ if srWin
            then (quant_dis sr (tcont .& srCMinusC)) .| (tucont .& srCPlusU)
            else (tcont .& srCPlusC)                 .| (quant_dis sr (tucont .& srCMinusU))

quant_dis :: (D.Rel c v a s, ?m :: c) => SynthesisRes c a -> a -> a
quant_dis SynthesisRes{..} rel = 
    trace "quant_dis"
    $ foldl' quant_dis1 rel
    $ map (\(v,ev) -> trace ("quant_dis " ++ D.mvarName v ++ "(" ++ show (D.mvarIdx v) ++ ") " ++ D.mvarName ev ++ "(" ++ show (D.mvarIdx ev) ++ ")") $ (D.mvarToVS v, D.mvarToVS ev))
    $ mapMaybe (\v -> fmap (v,) (find ((== (D.mkEnVarName $ D.mvarName v)) . D.mvarName) srLabelVars))
    $ srLabelVars

quant_dis1 :: (D.Rel c v a s, ?m :: c) => a -> (v,v) -> a
quant_dis1 rel (var, envar) = 
    let rel1 = trace "quant_dis1 rel1" $ ((eqConst envar (1::Int)) .& rel)
        rel2 = trace "quant_dis1 rel2" $ ((eqConst envar (0::Int)) .& {-exists var-} rel)
        rel3 = rel1 .| rel2
    in rel1 .| rel2
mkModel :: I.Spec -> 
           SMTSolver -> 
           SynthesisRes DdManager DdNode ->
           D.Model DdManager DdNode Store
mkModel spec solver sr = let ?spec    = spec 
                             ?solver  = solver
                         in mkModel' sr
mkModel' :: (?spec::I.Spec, ?solver::SMTSolver) => SynthesisRes DdManager DdNode -> D.Model DdManager DdNode Store
mkModel' sr@SynthesisRes{..} = model
    where
    mCtx                  = srCtx
    mStateVars            = srStateVars
    mUntrackedVars        = srUntrackedVars 
    mLabelVars            = srLabelVars
    mStateRels            = [ ("controllable"  , srCont)
                            , ("uncontrollable", let ?m = srCtx in nt srCont)
                            , ("init"          , srInit)] ++
                            zip (map I.goalName $ I.tsGoal $ I.specTran ?spec) srGoals  ++ 
                            zip (map I.fairName $ I.tsFair $ I.specTran ?spec) srFairs
    mTransRels            = [ (if' srWin "trel_win" "trel_lose" , mkTRel sr)
                            , ("trel"                           , srTran)
                            , ("c-c"                            , srCMinusC)
                            , ("c+c"                            , srCPlusC)
                            , ("c-u"                            , srCMinusU)
                            , ("c+u"                            , srCPlusU)]
    mViews                = []
    mConcretiseState      = concretiseS
    mConcretiseTransition = concretiseT
    mAutoConcretiseTrans  = True
    
    model = D.Model{..}

    concretiseS :: DdNode -> Maybe (D.State DdNode Store)
    concretiseS d =
        let ?m       = mCtx
            ?model   = model
            ?absvars = srAbsVars
        in D.concretiseState d

    concretiseT :: D.Transition DdNode Store -> Maybe (D.Transition DdNode Store)
    concretiseT D.Transition{..} | D.isConcreteState tranFrom = do
        let ?m       = mCtx
            ?model   = model
            ?absvars = srAbsVars
        cstate <- D.sConcrete tranFrom
        cnext  <- D.concretiseTransition cstate tranAbstractLabel (D.sAbstract tranTo)
        let tr' = D.abstractTransition tranFrom cnext
        if (D.sAbstract $ D.tranTo tr') .== (D.sAbstract tranTo)
           then return tr'
           else error "concretiseT: concretised next-state differs from abstract next-state"
                                 | otherwise = Nothing
