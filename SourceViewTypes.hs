{-# LANGUAGE RecordWildCards #-}

module SourceViewTypes where

import CFA
import Store
import DbgTypes

-- Magic block stack frame
data MBFrame = MBFrame {
    mbfEpoch :: Int,
    mbfLoc   :: Loc
} deriving (Eq)

instance Show MBFrame where
    show MBFrame{..} = "Loc: " ++ show mbfLoc ++ "(" ++ show mbfEpoch ++ ")"

-- Store extended with stack of magic blocks
data SVStore = SVStore { 
    sstStore   :: Store,
    sstMBStack :: [MBFrame]
} deriving (Eq)

instance Show SVStore where
    show SVStore{..} = "SVStore:" ++ show sstStore ++ "\n  MB stack:" ++ show sstMBStack

instance Vals SVStore
