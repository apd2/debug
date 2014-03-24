{-# LANGUAGE RecordWildCards, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances, UndecidableInstances, ImplicitParams, TupleSections, TemplateHaskell #-}

-- Interface to the abstraction library

module AbstractorIFace( SynthesisRes(..)
                      , mkSynthesisRes
                      , mkModel
                      , mkStrategy) where

import qualified Data.Map as M
import Data.List
import Data.Maybe
import Data.Tuple.Select
import Control.Monad
import Control.Monad.ST
import Control.Monad.Morph
import Control.Monad.Trans.Class
import Control.Monad.State

import Util
import Cudd
--import CuddExplicitDeref
import CuddConvert
import Interface hiding (db)
import TermiteGame
import Implicit
import BDDOps
import BddRecord hiding ((.&))
import Predicate
import Store
import SMTSolver
import TSLAbsGame
import qualified Spec            as F
import qualified ISpec           as I
import qualified TranSpec        as I
import qualified IType           as I
import qualified IVar            as I
import qualified DbgTypes        as D
import qualified DbgConcretise   as D
import qualified DbgAbstract     as D
import qualified StrategyView    as D
import qualified SourceView      as D
import qualified SourceViewTypes as D
import Resource

instance D.Rel DdManager VarData DdNode [[SatBit]] where
    relToDDNode _ n = toDDNode n

-- Statistics about concrete variables used in predicates:
-- (state vars, state bits, label vars, label bits)
type VarStats  = (Int, Int, Int, Int)

data SynthesisRes c a = SynthesisRes { srWin                :: Maybe Bool
                                     , srWinningRegionMay   :: DdNode
                                     , srWinningRegionMust  :: DdNode
                                     , srWinningRegionMay'  :: DdNode
                                     , srWinningRegionMust' :: DdNode
                                     , srStrat          :: [[DdNode]]  -- winning strategy or counterexample
                                     , srCtx            :: c
                                     , srStateVars      :: [D.ModelStateVar]
                                     , srUntrackedVars  :: [D.ModelVar]
                                     , srLabelVars      :: [D.ModelVar]
                                     , srInitVars       :: [D.ModelVar]
                                     , srAbsVars        :: M.Map String AbsVar
                                     , srCont           :: a
                                     , srInit           :: a
                                     , srGoals          :: [a]
                                     , srFairs          :: [a]
                                     , srTran           :: [a]
                                     , srStateLabConstr :: a
                                     , srInconsistent   :: a
                                     --, srConsistent     :: a
                                     --, srConsistent'    :: a
                                     , srCPlusC         :: a
                                     , srCMinusC        :: a
                                     , srCPlusU         :: a
                                     , srCMinusU        :: a
                                     , srInconsistentInit :: a
                                     --, srCPreCont       :: a
                                     --, srCPreUCont      :: a
                                     --, srCPre''         :: a
                                     --, srCPreOverUCont    :: a
                                     --, srCPreOverCont     :: a
                                     --, srCPreUnderUCont   :: a
                                     --, srCPreUnderCont    :: a

                                     --, srCpreOver         :: a
                                     --, srCpreUnder        :: a
                                     --, srReachUnder       :: a
                                     --, srFairUnder        :: a
                                     --, srReachOver        :: a
                                     --, srFairOver         :: a
                                     --, srSolveFairU     :: a
                                     --, srSolveReachU    :: a
--                                     , srSolveFairM     :: a
                                     , srStats          :: VarStats
                                     }

--cpreOverMy' :: (MonadResource (DDNode s u) (ST s) t) => Ops s u -> SectionInfo s u -> RefineStatic s u -> RefineDynamic s u -> DDNode s u -> Lab s u -> DDNode s u -> t (ST s) (DDNode s u)
--cpreOverMy' ops si rs rd@RefineDynamic{..} hasOutgoingsCont labelPreds = cpre'' ops si rs rd hasOutgoingsCont labelPreds consistentPlusCULCont consistentPlusCULUCont

--cPreOverMy :: (MonadResource (DDNode s u) (ST s) t) => Ops s u -> SectionInfo s u -> RefineStatic s u -> RefineDynamic s u -> DDNode s u -> Lab s u -> DDNode s u -> t (ST s) (DDNode s u)
--cPreOverMy ops@Ops{..} = cPreHelper cpreOverMy' bforall ops  


mkSynthesisRes :: (MonadResource (DDNode s u) (ST s) mr) => I.Spec -> STDdManager s u -> (Maybe Bool, RefineInfo s u AbsVar AbsVar AbsPriv) -> mr (ST s) (SynthesisRes DdManager DdNode) 
mkSynthesisRes spec m (res, ri@RefineInfo{..}) = do
    let ?spec = spec 
        ?m    = toDdManager m
    let RefineStatic{..}  = rs
        RefineDynamic{..} = rd
        pdb               = db
    -- Compute state-label constraint
    let ops@Ops{..} = constructOps m
    stateLabelExpr <- flip evalStateT pdb $ hoist lift $ doUpdate ops (tslStateLabelConstraintAbs ?spec m)
    inconsistent   <- flip evalStateT pdb $ hoist lift $ doUpdate ops (tslInconsistent ?spec m)
    srStrat <- case res of
                  Just True -> liftM (map (map (toDdNode ?m)) . snd) $ strat ri
--                               do s0 <- strat ri
--                                  lift $ mapM (mapM (\s -> do s' <- bor m s $ bnot cont
--                                                              deref m s
--                                                              return $ toDdNode ?m s')) s0  -- don't restrict uncontrollable behaviour
                  Just False -> liftM (map (map (toDdNode ?m))) $ cex ri
                                --   lift $ mapM (mapM (\s -> do s' <- bor m s cont
                                --                               deref m s
                                --                               return $ toDdNode ?m s')) s0  -- don't restrict controllable behaviour
                  Nothing    -> --liftM (map (map (toDdNode ?m))) $ cexLiberalEnv ri
                                return [[t]]

    let SectionInfo{..} = _sections pdb
        SymbolInfo{..}  = _symbolTable pdb
        labelVars = map (\l -> (sel1 l, sel3 l)) $ M.elems _labelVars
    --consSU           <- lift $ bexists _untrackedCube consistentNoRefine 
    --consS            <- lift $ bexists _labelCube     consSU
    --lift $ deref consSU
    --consistent'   <- lift $ mapVars consS
    --lift $ deref consS
    --hasOutgoings <- doHasOutgoings ops trans
    --cPreCont     <- cpreCont'  ops si rd labelVars cont hasOutgoings wu


    --fairorwinunder   <- $r2 bor wu (fair !! 0)
    --fairorwinover    <- $r2 bor wo (fair !! 0)
    --goalorfair       <- $r2 bor (goal !! 0) (fair !! 0)
--    goalorwin    <- $r2 bor wn (goal !! 0)

    --stratUCont   <- cpreUCont' ops si rd labelVars cont fairorwinover
    --cPreUOver    <- liftM bnot $ $r2 (andAbstract _labelCube) consistentMinusCULUCont stratUCont
    --stratCont    <- cpreCont' ops si rd labelVars cont hasOutgoings fairorwinover
    --cPreCOver    <- $r2 (andAbstract _labelCube) consistentPlusCULCont stratCont

    --stratUCont   <- cpreUCont' ops si rd labelVars cont fairorwinunder
    --cPreUUnder   <- liftM bnot $ $r2 (andAbstract _labelCube) consistentPlusCULUCont stratUCont
    --stratCont    <- cpreCont' ops si rd labelVars cont hasOutgoings fairorwinunder
    --cPreCUnder   <- $r2 (andAbstract _labelCube) consistentMinusCULCont stratCont

    --cPre''       <- cpre'' ops si rs rd hasOutgoings labelVars consistentPlusCULCont consistentMinusCULUCont fairorwinover
    --cpreOver     <- cPreOver ops si rs rd hasOutgoings labelVars fairorwinover
    --cpreUnder    <- cPreUnder ops si rs rd hasOutgoings labelVars fairorwinunder

    --fairUnder    <- solveFair  (cPreUnder ops si rs rd hasOutgoings labelVars) ops rs btrue wu (fair !! 0)
    --reachUnder   <- solveReach (cPreUnder ops si rs rd hasOutgoings labelVars) ops rs btrue (goal !! 0) bfalse

    --fairOver     <- solveFair  (cPreOver ops si rs rd hasOutgoings labelVars) ops rs btrue wo (fair !! 0)
    --reachOver    <- solveReach (cPreOver ops si rs rd hasOutgoings labelVars) ops rs btrue (goal !! 0) bfalse

--    cPre''       <- cPreUnder ops si rs rd hasOutgoings labelVars fairorwin
--
--
    --fairWinU     <- solveFair (cPreUnder ops si rs rd hasOutgoings labelVars)  ops rs btrue wu (fair !! 0)
--    fairWinM     <- solveFair (cPreOverMy ops si rs rd hasOutgoings labelVars) ops rs btrue wn (fair !! 0)
--    $d deref fairorwin
--    $d deref goalorwin

--    tgt'         <- $r2 band (goal !! 0) buchiWinning
    --reachUnder   <- solveReach (cPreUnder ops si rs rd hasOutgoings labelVars) ops rs btrue (goal !! 0) bfalse
--    liftBDD $ $d deref tgt'
--
--    winNoConstraint <- cpreCont' ops si rd labelVars cont hasOutgoings reachUnder
    wo' <- lift $ mapVars wo
    wu' <- lift $ mapVars wu

    -- let srConsistent = toDdNode ?m consistentNoRefine
    let (state, untracked) = partition func $ M.toList _stateVars
            where func (_, (_, is, _, _)) = not $ null $ intersect is _trackedInds

        --srConsistent'   = toDdNode ?m consistent'
        --srCPreCont      = toDdNode ?m cPreCont
        --srCPreUCont     = toDdNode ?m cPreUCont
        --srCPre''        = toDdNode ?m cPre''
        srWin           = res 
        srCtx           = ?m
        ps = pairs state
        check = any (\((p1,_),(p2,_)) -> show p1 == show p2) ps
        srStateVars     = if check
                             then error "found identical predicates"
                             else map toTupleState state
            where toTupleState        (p, (_, is, _, is')) = (show p, avarType p, (is, is'))
        srUntrackedVars = map toModelVarUntracked untracked
            where toModelVarUntracked (p, (_, is, _, _)) = D.ModelVar (show p) (avarType p) is
        srLabelVars     = concatMap toModelVarLabel $ M.toList _labelVars
            where toModelVarLabel     (p, (_, is, _, ie)) = [D.ModelVar (show p) (avarType p) is, D.ModelVar (show p ++ ".en") D.Bool [ie]]
        srInitVars      = map toModelVarInit $ M.toList _initVars
            where toModelVarInit      (p, (_, is, _, _)) = D.ModelVar (show p) (avarType p) is
        srAbsVars       = M.fromList $ map (\v -> (show v,v)) $ (M.keys _stateVars) ++ (M.keys _labelVars) ++ (M.keys _initVars)
        srWinningRegionMay   = toDdNode srCtx wo
        srWinningRegionMust  = toDdNode srCtx wu
        srWinningRegionMay'  = toDdNode srCtx wo'
        srWinningRegionMust' = toDdNode srCtx wu'

        srCont          = toDdNode srCtx cont
        srInit          = toDdNode srCtx init
        srGoals         = map (toDdNode srCtx) goal
        srFairs         = map (toDdNode srCtx) fair
        srTran          = {-conj $-} map (toDdNode srCtx . snd) trans
        srStateLabConstr = toDdNode srCtx stateLabelExpr
        srInconsistent  = toDdNode srCtx inconsistent
        srCMinusC       = toDdNode srCtx consistentMinusCULCont
        srCPlusC        = toDdNode srCtx consistentPlusCULCont
        srCMinusU       = toDdNode srCtx consistentMinusCULUCont
        srCPlusU        = toDdNode srCtx consistentPlusCULUCont
        srInconsistentInit = toDdNode srCtx inconsistentInit
        (svars, lvars)  = partition ((==I.VarState) . I.varCat) 
                          $ nub
                          $ concat [ concatMap (avarVar . fst) state
                                   , concatMap (avarVar . fst) untracked
                                   , concatMap (avarVar . fst) $ M.toList _labelVars]
        sbits           = sum $ map I.typeWidth svars
        lbits           = sum $ map I.typeWidth lvars
        srStats         = (length svars, sbits, length lvars, lbits)
        --srCpreOver      = toDdNode srCtx cpreOver
        --srCpreUnder     = toDdNode srCtx cpreUnder
        --srCPreOverUCont = toDdNode srCtx cPreUOver
        --srCPreOverCont  = toDdNode srCtx cPreCOver
        --srCPreUnderUCont = toDdNode srCtx cPreUUnder
        --srCPreUnderCont  = toDdNode srCtx cPreCUnder

        --srReachUnder    = toDdNode srCtx reachUnder
        --srFairUnder     = toDdNode srCtx fairUnder
        --srReachOver     = toDdNode srCtx reachOver
        --srFairOver      = toDdNode srCtx fairOver

 
        --srSolveFairU    = toDdNode srCtx fairWinU
        --srSolveReachU   = toDdNode srCtx reachUnder
        --srSolveFairM    = toDdNode srCtx fairWinM

    return SynthesisRes{..}

-- Extract type information from AbsVar
avarType :: (?spec::I.Spec) => AbsVar -> D.Type
avarType (AVarPred _)  = D.Bool
avarType (AVarBool tr) = D.Bool
avarType (AVarEnum tr) = D.Enum $ (I.enumEnums $ I.getEnumeration n) where I.Enum n = I.typ tr
avarType (AVarInt  tr) = case I.typ tr of
                              I.SInt w -> D.SInt w
                              I.UInt w -> D.UInt w

---- Construct transition relation that represent the result of three-valued 
---- abstraction.
----
---- If srWin == True then
----   T := quantify_dis (Tc /\ c-c) \/ (Tu /\ c+u)
---- else
----   T := (Tc /\ c+c) \/ quantify_dis (Tc /\ c-u)
----
---- where Tc and Tu are controllable and uncontrollable transition relations,
---- and the quantify_dis function quantifies away disabled variables.  For a 
---- single variable X:
----
---- quantify_dis_X(rel) = (X.en /\ rel) \/ (!X.en /\ exists X. rel)
---- 
--mkTRel :: (D.Rel c v a s) => SynthesisRes c a -> a
--mkTRel sr@SynthesisRes{..} =
--    let ?m     = srCtx in
--    let tcont  = srTran .& srCont
--        tucont = srTran .& (nt srCont)
--    in trace "mkTRel"
--       $ case srWin of
--            Just True  -> (quant_dis sr (tcont .& srCMinusC)) .| (tucont .& srCPlusU)
--            Just False -> (tcont .& srCPlusC)                 .| (quant_dis sr (tucont .& srCMinusU))
--            Nothing    -> srTran
--
--quant_dis :: (D.Rel c v a s, ?m :: c) => SynthesisRes c a -> a -> a
--quant_dis SynthesisRes{..} rel = 
--    foldl' quant_dis1 rel
--    $ map (\(v,ev) -> trace ("quant_dis " ++ D.mvarName v ++ "(" ++ show (D.mvarIdx v) ++ ") " ++ D.mvarName ev ++ "(" ++ show (D.mvarIdx ev) ++ ")") $ (D.mvarToVS v, D.mvarToVS ev))
--    $ mapMaybe (\v -> fmap (v,) (find ((== (D.mkEnVarName $ D.mvarName v)) . D.mvarName) srLabelVars))
--    $ srLabelVars
--
--quant_dis1 :: (D.Rel c v a s, ?m :: c) => a -> (v,v) -> a
--quant_dis1 rel (var, envar) = 
--    let rel1 = ((eqConst envar (1::Int)) .& rel)
--        rel2 = ((eqConst envar (0::Int)) .& exists var rel)
--        --rel3 = rel1 .| rel2
--    in rel1 .| rel2

mkModel :: F.Spec -> 
           F.Spec ->
           I.Spec -> 
           SMTSolver -> 
           SynthesisRes DdManager DdNode ->
           D.Model DdManager DdNode Store D.SVStore
mkModel inspec flatspec spec solver sr = let ?spec     = spec 
                                             ?flatspec = flatspec
                                             ?inspec   = inspec
                                             ?solver   = solver
                                         in mkModel' sr

mkModel' :: (?inspec::F.Spec, ?flatspec::F.Spec, ?spec::I.Spec, ?solver::SMTSolver) => SynthesisRes DdManager DdNode -> D.Model DdManager DdNode Store D.SVStore
mkModel' sr@SynthesisRes{..} = model
    where
    mCtx                  = srCtx
    mStateVars            = srStateVars
    mUntrackedVars        = srUntrackedVars 
    mLabelVars            = srLabelVars
    mInitVars             = srInitVars
    mStateRels            = [ (D.contRelName   , srCont)
                            , ("win+"           , srWinningRegionMay)
                            , ("win-"           , srWinningRegionMust)
                            --, ("uncontrollable", let ?m = srCtx in nt srCont)
                            , ("init"          , if' (srWin == Just True || srWin == Nothing) 
                                                     srInit 
                                                     (let ?m = srCtx in srInit .& (nt srWinningRegionMust) {- .& srConsistentNxt-}))] ++
                            zip (map I.goalName $ I.tsGoal $ I.specTran ?spec) srGoals  {- ++ 
                            zip (map I.fairName $ I.tsFair $ I.specTran ?spec) srFairs -}
    mTransRels            = [ {- (case srWin of 
                                    Just True  -> "trel_win" 
                                    Just False -> "trel_lose" 
                                    Nothing    -> "trel", 
                               mkTRel sr)-}
                              ("trel"                           , True                , let ?m = srCtx in (conj srTran))
                            --, ("consistentNoRefine"             , True                , srConsistent)
                            --, ("consistentNoRefine'"            , True                , srConsistent')
                            , ("stateLabelConstr"               , True                , srStateLabConstr)
                            , ("nt srInconsistent"              , True                , let ?m = srCtx in nt srInconsistent)
                            , ("c-c"                            , srWin == Just True  , srCMinusC)
                            , ("c+c"                            , True                , srCPlusC)
                            , ("c-u"                            , srWin == Just False , srCMinusU)
                            , ("c+u"                            , True                , srCPlusU)
                            , ("win+"                           , False               , srWinningRegionMay)
                            , ("win-"                           , False               , srWinningRegionMust)
                            , ("win+'"                          , False               , srWinningRegionMay')
                            , ("win-'"                          , False               , srWinningRegionMust')
                            , ("nt win+"                        , False               , let ?m = srCtx in nt srWinningRegionMay)
                            , ("nt win-"                        , False               , let ?m = srCtx in nt srWinningRegionMust)
                            , ("nt win+'"                       , False               , let ?m = srCtx in nt srWinningRegionMay')
                            , ("nt win-'"                       , False               , let ?m = srCtx in nt srWinningRegionMust')
                            , ("nt inconsistentInit"            , True                , let ?m = srCtx in nt srInconsistentInit)
                        --    , ("cpreOver"                       , False               , srCpreOver)
                       --     , ("cpreUnder"                      , False               , srCpreUnder)
                       --     , ("cpreOverUCont"                  , False               , srCPreOverUCont)
                       --     , ("cpreOverCont"                   , False               , srCPreOverCont)
                       --     , ("cpreUnderUCont"                 , False               , srCPreUnderUCont)
                       --     , ("cpreUnderCont"                  , False               , srCPreUnderCont)
                       --     , ("reachUnder"                     , False               , srReachUnder)
                       --     , ("fairUnder"                      , False               , srFairUnder)
                       --     , ("reachOver"                      , False               , srReachOver)
                       --     , ("fairOver"                       , False               , srFairOver)
 
                       --     , ("cpreCont win"                   , False               , srCPreCont)
                       --     , ("cpreUCont win"                  , False               , srCPreUCont)
                       --     , ("nt srCPre''"                    , False               , let ?m = srCtx in nt srCPre'')
                       --     , ("srCPre''"                       , False               , srCPre'')
                       --     , ("solveFair cPreUnder"            , False               , srSolveFairU)
                       --     , ("solveReach cPreUnder"           , False               , srSolveReachU)
                       --     , ("nt solveReach cPreUnder"        , False               , let ?m = srCtx in nt srSolveReachU)
                          --  , ("solveFair cPreMy"               , False               , srSolveFairM)
                            ] 
                            -- ++ zip3 (map show [(0::Int)..]) (repeat False) srTran
    mViews                = []
    mConcretiseState      = concretiseS
    mConcretiseTransition = concretiseT
    mAutoConcretiseTrans  = True
    mConstraints          = M.empty
    mTransRel             = let ?m = srCtx in b
    mOracles              = []
    mStrategy             = Nothing

    model = D.Model{..}

    concretiseS :: DdNode -> Maybe (D.State DdNode D.SVStore)
    concretiseS d =
        let ?m       = mCtx
            ?model   = model
            ?absvars = srAbsVars
        in D.concretiseState d

    concretiseT :: D.Transition DdNode Store D.SVStore-> Maybe (D.Transition DdNode Store D.SVStore)
    concretiseT D.Transition{..} | D.isConcreteState tranFrom = do
        let ?m       = mCtx
            ?model   = model
            ?absvars = srAbsVars
        cstate <- D.sConcrete tranFrom
        cnext  <- D.concretiseTransition (D.sstStore $ fst cstate) tranAbstractLabel
        let tr' = D.abstractTransition tranFrom cnext []
        let msrc = D.contTransToSource ?inspec ?flatspec ?spec tr'
        return tr'{D.tranSrc = msrc}
--        if ((D.sAbstract $ D.tranTo tr') .-> (D.sAbstract tranTo)) .== t
--           then return tr'
--           else error "concretiseT: concretised next-state differs from abstract next-state"
                                 | otherwise = Nothing


mkStrategy :: I.Spec -> SynthesisRes DdManager DdNode -> Maybe (D.Strategy DdNode)
mkStrategy spec SynthesisRes{..} = Just D.Strategy{..}
    where
    stratName  = if' (srWin == Just True) "Winning strategy" "Counterexample strategy"
    stratGoals = zip (map I.goalName $ I.tsGoal $ I.specTran spec) srGoals
    stratFair  = zip (map I.fairName $ I.tsFair $ I.specTran spec) srFairs
    stratRel   = srStrat
