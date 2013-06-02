{-# LANGUAGE RecordWildCards, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances, UndecidableInstances #-}

-- Interface to the abstraction library

module AbstractorIFace(mkModel) where

import qualified Data.Map as Map
import Data.List

import Cudd
import CuddConvert
import DbgTypes
import Interface
import TermiteGame
import CuddExplicitDeref

mkModel :: (Show sp, Show lp) => STDdManager s u -> RefineStatic s u -> RefineDynamic s u -> SectionInfo s u -> SymbolInfo s u sp lp -> Model DdManager DdNode b
mkModel m RefineStatic{..} RefineDynamic{..} SectionInfo{..} SymbolInfo{..} = Model {..} 
    where
    mCtx           = toDdManager m
    (state, untracked) = partition func $ Map.toList _stateVars
        where func (_, (_, is, _, _)) = not $ null $ intersect is _trackedInds
    mStateVars     = map toTupleState state
        where toTupleState        (p, (_, is, _, is')) = (show p, UInt (length is), (is, is'))
    mUntrackedVars = map toModelVarUntracked untracked
        where toModelVarUntracked (p, (_, is, _, _)) = ModelVar (show p) (UInt (length is)) is
    mLabelVars     = concatMap toModelVarLabel $ Map.toList _labelVars
        where toModelVarLabel     (p, (_, is, _, ie)) = [ModelVar (show p) (UInt (length is)) is, ModelVar (show p ++ ".en") (UInt 1) [ie]]
    mStateRels = concat $ [[("cont", toDdNode mCtx cont)], [("init", toDdNode mCtx init)], goals, fairs]
        where
        goals = zipWith (func "goal") [0..] goal
        fairs = zipWith (func "fair") [0..] fair
        func :: String -> Int -> DDNode s u -> (String, DdNode)
        func prefix idx node = (prefix ++ show idx, toDdNode mCtx node)
    mTransRels = [("trans", toDdNode mCtx trans), ("c-c", toDdNode mCtx consistentMinusCULCont), ("c+c", toDdNode mCtx consistentPlusCULCont), ("c-u", toDdNode mCtx consistentMinusCULUCont), ("c+u", toDdNode mCtx consistentPlusCULUCont)]
    mViews     = []
