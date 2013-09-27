{-# LANGUAGE RecordWildCards #-}

module CodeWin(MBID(..),
               mbidChild,
               MBDescr(..),
               MBActive(..),
               MBInactive(..),
               isMBICurrent,
               mbiGetRegionText,
               mbEpoch,
               CodeWin,
               RCodeWin,
               cwLookupMB,
               codeWinNew,
               codeWinWidget,
               codeWinActiveMB,
               codeWinLookupMB,
               codeWinGetMB,
               codeWinMBRefresh,
               codeWinMBDeactivate,
               codeWinMBActivate,
               codeWinSetSelection,
               codeWinClearSelection) where

import qualified Data.Map        as M
import qualified Graphics.UI.Gtk as G
import Data.IORef
import Data.Maybe
import Data.List
import Text.Parsec
import Control.Monad

import Util
import Pos
import qualified Spec            as F
import qualified Template        as F
import qualified Method          as F
import qualified Statement       as F
import qualified Process         as F
import CodeWidgetTypes
import CodeWidget
import CFA

-- Magic block descriptor

-- MBActive - magic block currently being executed
data MBActive   = MBActive { mbaRegion :: Region            -- contains current MB text, which does not change
                           , mbaEpoch  :: Int               -- version of MB contents
                           , mbaCFA    :: CFA               -- compiled representation
                           , mbaNested :: M.Map Loc MBDescr
                           }

-- MBInactive - magic block that is not currently executing and that has not 
-- changed since the last execution
data MBInactive = MBICurrent { mbiRegion :: Either Region String -- contains MB text with all subregions inlined in it
                             , mbiEpoch  :: Int
                             , mbiText   :: String               -- region contents before it became inactive 
                             , mbiCFA    :: CFA                  -- compiled representation of mbiText
                             , mbiNested :: M.Map Loc MBInactive -- all children of inactive region are inactive
                             }
                | MBIStale   { mbiRegion :: Either Region String
                             , mbiEpoch  :: Int
                             }

isMBICurrent :: MBInactive -> Bool
isMBICurrent (MBICurrent _ _ _ _ _) = True
isMBICurrent _                      = False

mbiGetRegionText :: RCodeWin -> MBInactive -> IO String
mbiGetRegionText ref mbi = do
    cw <- readIORef ref
    case mbiRegion mbi of
         Left reg -> regionGetText (cwAPI cw) reg
         Right s  -> return s

data MBDescr = MBA MBActive
             | MBI MBInactive

isMBActive :: MBDescr -> Bool
isMBActive (MBA _) = True
isMBActive _       = False

mbEpoch :: MBDescr -> Int
mbEpoch (MBA mba) = mbaEpoch mba
mbEpoch (MBI mbi) = mbiEpoch mbi

data MBID    = MBID Pos [Loc] deriving (Eq, Ord)
--data MBEpoch = MBEpoch Int [Int]

mbidChild :: MBID -> Loc -> MBID
mbidChild (MBID p ls) l = MBID p (ls ++ [l])

mbidParent :: MBID -> Maybe MBID
mbidParent (MBID _ [])  = Nothing
mbidParent (MBID p ls)  = Just $ MBID p $ init ls

isParentOf :: MBID -> MBID -> Bool
isParentOf (MBID p1 ls1) (MBID p2 ls2) = (p1 == p2) && length ls1 < length ls2 && isPrefixOf ls1 ls2

data CodeWin = CodeWin { cwAPI       :: CwAPI
                       , cwView      :: G.Widget
                       , cwScroll    :: G.ScrolledWindow                     -- top-level widget
                       , cwFiles     :: M.Map SourceName (Region, G.TextTag) -- Source files and corresponding regions and tags
                       , cwMBRoots   :: M.Map Pos MBDescr                    -- Roots of the MB hierarchy
                       , cwActiveMB  :: Maybe MBID                           -- Currently active MB
                       , cwSelection :: Maybe (Region, G.TextTag)
                       }

cwLookupMB :: CodeWin -> MBID -> Maybe MBDescr
cwLookupMB cw (MBID p ls) = lookupMB (cwMBRoots cw M.! p) ls

lookupMB :: MBDescr -> [Loc] -> Maybe MBDescr
lookupMB mb        []     = Just mb
lookupMB (MBA mba) (l:ls) = maybe Nothing (\mb'  -> lookupMB mb'        ls) $ M.lookup l (mbaNested mba)
lookupMB (MBI mbi) (l:ls) = maybe Nothing (\mbi' -> lookupMB (MBI mbi') ls) $ M.lookup l (mbiNested mbi)

cwGetMB :: CodeWin -> MBID -> MBDescr
cwGetMB cw mbid = fromJust $ cwLookupMB cw mbid

cwMBChildren :: CodeWin -> MBID -> [MBID]
cwMBChildren cw mbid = map (mbidChild mbid) 
                       $ case cwGetMB cw mbid of
                              MBA mba -> map fst $ M.toList $ mbaNested mba
                              MBI mbi -> map fst $ M.toList $ mbiNested mbi

cwSetMB :: CodeWin -> MBID -> MBDescr -> CodeWin
cwSetMB cw (MBID p ls) mb = cw {cwMBRoots = M.adjust (\mb' -> setMB mb' ls mb) p (cwMBRoots cw)}

setMB :: MBDescr -> [Loc] -> MBDescr -> MBDescr
setMB _         []     mb' = mb'
setMB (MBA mba) (l:ls) mb' = MBA $ mba {mbaNested = M.adjust (\mb''  -> setMB mb'' ls mb') l (mbaNested mba)}
setMB (MBI mbi) (l:ls) mb' = MBI $ mbi {mbiNested = M.adjust (\mbi'' -> let MBI mbi''' = setMB (MBI mbi'') ls mb' in mbi''') l (mbiNested mbi)}


type RCodeWin = IORef CodeWin

-- Load all spec files and initialise cwMBRoots
codeWinNew :: F.Spec -> IO RCodeWin
codeWinNew spec@F.Spec{..} = do
    -- Source window at the top
    cwScroll <- G.scrolledWindowNew Nothing Nothing
    G.widgetShow cwScroll
    Code cwAPI cwView <- codeWidgetNew "tsl" 600 600
    G.containerAdd cwScroll cwView

    ref <- newIORef $ error "codeWinNew: undefined"

    let files = nub 
                $ map (sourceName . fst)
                $ map pos specTemplate ++ map pos specType ++ map pos specConst
    cwFiles <- liftM M.fromList 
               $ mapM (\n -> do reg <- pageCreate cwAPI n
                                tag <- tagNew cwAPI reg
                                return (n, (reg, tag))) files
    cwMBRoots <- liftM M.fromList
                 $ mapM (\p -> do let parent = fst $ cwFiles M.! (sourceName $ fst p)
                                  reg <- regionCreateFrom cwAPI parent p True (editCB ref $ MBID p [])
                                  return (p, MBI $ MBIStale (Left reg) 0)) 
                 $ specFindMBs spec
    writeIORef ref $ CodeWin{cwActiveMB = Nothing, cwSelection = Nothing, ..}
    return ref

codeWinWidget :: RCodeWin -> IO G.Widget
codeWinWidget = liftM G.toWidget . getIORef cwScroll

codeWinActiveMB :: RCodeWin -> IO (Maybe MBID)
codeWinActiveMB = getIORef cwActiveMB

codeWinLookupMB :: RCodeWin -> MBID -> IO (Maybe MBDescr)
codeWinLookupMB ref mbid = getIORef (\cw -> cwLookupMB cw mbid) ref

codeWinGetMB :: RCodeWin -> MBID -> IO MBDescr
codeWinGetMB ref mbid = getIORef (\cw -> cwGetMB cw mbid) ref

-- Make stale MB active, creating nested MBs if necessary
-- Assumes: MBID refers to an existing stale MB whose parent is active.
codeWinMBRefresh :: RCodeWin -> MBID -> CFA -> IO ()
codeWinMBRefresh ref mbid cfa = do
    cw@CodeWin{..} <- readIORef ref
    let MBI MBIStale{mbiRegion = Left ireg, ..} = cwGetMB cw mbid
    regionEditable cwAPI ireg False
    nested <- mapM (\loc -> do let p = cfaGetMBPos cfa loc
                               reg <- regionCreateFrom cwAPI ireg p True (editCB ref $ mbidChild mbid loc)
                               return (loc, MBI $ MBIStale (Left reg) 0))
              $ cfaFindMBs cfa
    let mb' = MBA $ MBActive ireg mbiEpoch cfa (M.fromList nested)
    writeIORef ref $ cwSetMB cw mbid mb'

-- Deactivate all active MBs
codeWinMBDeactivate :: RCodeWin -> IO ()
codeWinMBDeactivate ref = do
    CodeWin{..} <- readIORef ref
    maybe (return ()) 
          (\(MBID p _) -> deactivateRec ref (MBID p []))
          cwActiveMB

-- Activate MB
-- Assumes: MBID refer to an existing non-stale MB
codeWinMBActivate :: RCodeWin -> MBID -> IO ()
codeWinMBActivate ref mbid@(MBID p locs) = do
    CodeWin{..} <- readIORef ref
    let Just mbid'@(MBID _ locs') = cwActiveMB
    (if' (Just mbid == cwActiveMB)                      (return ())
     $ if' (isJust cwActiveMB && isParentOf mbid mbid') (deactivateRec ref (MBID p (take (length locs + 1) locs'))) 
     $ if' (isJust cwActiveMB && isParentOf mbid' mbid) (do _ <- mapM (\ls -> activate ref (MBID p ls)) $ drop (length locs') $ inits locs
                                                            return ())
     $ do codeWinMBDeactivate ref
          _ <- mapM (\ls -> activate ref (MBID p ls)) $ inits locs
          return ())

-- Select range within currently active region
codeWinSetSelection :: RCodeWin -> Maybe MBID -> Pos -> String -> IO ()
codeWinSetSelection ref mmbid p color = do
    codeWinClearSelection ref
    cw@CodeWin{..} <- readIORef ref
    let (reg, tag) = maybe (cwFiles M.! (sourceName $ fst p))
                           (\mbid@(MBID (p',_) _) -> let MBA mba = cwGetMB cw mbid 
                                                     in (mbaRegion mba, snd $ cwFiles M.! sourceName p'))
                           mmbid
    G.set tag [G.textTagBackground G.:= color]
    regionApplyTag cwAPI reg tag p
    writeIORef ref $ cw {cwSelection = Just (reg,tag)}

codeWinClearSelection :: RCodeWin -> IO ()
codeWinClearSelection ref = do
    CodeWin{..} <- readIORef ref
    maybe (return ())
          (\(reg, tag) -> regionRemoveTag cwAPI reg tag)
          cwSelection

editCB :: RCodeWin -> MBID -> IO ()
editCB ref mbid = do
    cw <- readIORef ref
    let MBI mbi = cwGetMB cw mbid
    case mbi of 
         MBIStale{..}   -> return ()
         MBICurrent{..} -> writeIORef ref $ cwSetMB cw mbid $ MBI $ MBIStale mbiRegion (mbiEpoch + 1)

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

--cwPosToRegion :: CodeWin -> Pos -> Region
--cwPosToRegion cw@CodeWin{..} pos = 
--    maybe (cwFiles M.! (sourceName $ fst pos))
--          (\mbid -> let MBA mba = cwGetMB cw mbid in mbaRegion mba)
--          cwActive

-- deactivate active MB and, recursively, its children
deactivateRec :: RCodeWin -> MBID -> IO ()
deactivateRec ref mbid = do
   cw <- readIORef ref
   _ <- mapM (deactivateRec ref) 
        $ filter (\mbid' -> isMBActive $ cwGetMB cw mbid')
        $ cwMBChildren cw mbid
   deactivate ref mbid

-- deactivate active MB; all children are assumed to be inactive
deactivate :: RCodeWin -> MBID -> IO ()
deactivate ref mbid = do
    cw@CodeWin{..} <- readIORef ref
    let MBA (MBActive{..}) = cwGetMB cw mbid
    text <- regionGetText cwAPI mbaRegion
    -- Delete nested regions and merge text into the parent
    alltext <- regionGetAllText cwAPI mbaRegion
    nested <- mapM (\(loc, MBI mbi) -> do let Left reg = mbiRegion mbi
                                          text' <- regionGetAllText cwAPI reg
                                          regionDelete cwAPI reg
                                          return $ (loc, mbi {mbiRegion = Right text'}))
              $ M.toList mbaNested
    regionSetText cwAPI mbaRegion alltext
    regionEditable cwAPI mbaRegion True
    let mb' = MBI $ MBICurrent (Left mbaRegion) mbaEpoch text mbaCFA (M.fromList nested)
    writeIORef ref $ (\cw' -> cw' {cwActiveMB = mbidParent mbid})
                   $ cwSetMB cw mbid mb'

-- activate inactive MB whose parent is already active
activate :: RCodeWin -> MBID -> IO ()
activate ref mbid = do
    cw@CodeWin{..} <- readIORef ref
    let MBI MBICurrent{mbiRegion=Left reg, ..} = cwGetMB cw mbid
    regionSetText cwAPI reg mbiText
    regionEditable cwAPI reg False
    -- Create and populate nested regions
    nested <- mapM (\(loc, mbi) -> do let Right content = mbiRegion mbi
                                      reg' <- regionCreateFrom cwAPI reg (cfaGetMBPos mbiCFA loc) True (editCB ref $ mbidChild mbid loc)
                                      regionSetText cwAPI reg' content
                                      return (loc, MBI $ mbi{mbiRegion = Left reg'}))
              $ M.toList mbiNested
    writeIORef ref $ (\cw' -> cw' {cwActiveMB = Just mbid})
                   $ cwSetMB cw mbid 
                   $ MBA $ MBActive reg mbiEpoch mbiCFA (M.fromList nested)

-- Listing all MBs in a spec
specFindMBs :: F.Spec -> [Pos]
specFindMBs spec = concatMap tmFindMBs $ F.specTemplate spec
 
tmFindMBs :: F.Template -> [Pos]
tmFindMBs tm = concat $ map (statFindMBs . F.procStatement) (F.tmProcess tm) ++ map methFindMBs (F.tmMethod tm)

methFindMBs :: F.Method -> [Pos]
methFindMBs meth = case F.methBody meth of
                        Left (mb, ma) -> concatMap statFindMBs $ catMaybes [mb, ma]
                        Right s       -> statFindMBs s

statFindMBs :: F.Statement -> [Pos]
statFindMBs (F.SSeq     _ ss)      = concatMap statFindMBs ss
statFindMBs (F.SPar     _ ps)      = concatMap (statFindMBs . snd) ps
statFindMBs (F.SForever _ b)       = statFindMBs b
statFindMBs (F.SDo      _ b _)     = statFindMBs b
statFindMBs (F.SWhile   _ _ b)     = statFindMBs b
statFindMBs (F.SFor     _ _ b)     = statFindMBs b
statFindMBs (F.SChoice  _ ss)      = concatMap statFindMBs ss
statFindMBs (F.SITE     _ _ t me)  = concatMap statFindMBs $ t : maybeToList me 
statFindMBs (F.SCase    _ _ cs md) = concatMap statFindMBs $ map snd cs ++ maybeToList md
statFindMBs (F.SMagic   _ p _)     = [p]
statFindMBs _                      = []


cfaGetMBPos :: CFA -> Loc -> Pos
cfaGetMBPos cfa l = p
    where ActStat (F.SMagic _ p _) = locAct $ cfaLocLabel l cfa

cfaFindMBs :: CFA -> [Loc]
cfaFindMBs cfa = nub 
                 $ filter (\l -> case locAct $ cfaLocLabel l cfa of
                                      ActStat (F.SMagic _ _ _) -> True
                                      _                        -> False)
                 $ cfaDelayLocs cfa
