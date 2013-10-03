{-# LANGUAGE RecordWildCards #-}

module CodeWin(MBID(..),
               mbidChild,
               mbidParent,
               mbidFile,
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
               codeWinPos,
               codeWinActiveMB,
               codeWinLookupMB,
               codeWinGetMB,
               codeWinSaveAll,
               codeWinModifiedFiles,
               codeWinSetMBText,
               codeWinMBRefresh,
               codeWinMBActivate,
               codeWinSetSelection,
               codeWinClearSelection) where

import qualified Data.Map        as M
import qualified Graphics.UI.Gtk as G
import Data.IORef
import Data.Maybe
import Data.List
import Data.Tuple.Select
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
                           , mbaText   :: String            -- region source code
                           , mbaCFA    :: CFA               -- compiled representation of mbaText
                           , mbaNested :: M.Map Loc MBDescr
                           } deriving (Show)

-- MBInactive - magic block that is not currently executing and that has not 
-- changed since the last execution
data MBInactive = MBICurrent { mbiRegion :: Either Region String -- contains MB text with all subregions inlined in it
                             , mbiEpoch  :: Int
                             , mbiText   :: String               -- same as mbaText
                             , mbiCFA    :: CFA                  -- compiled representation of mbiText
                             , mbiNested :: M.Map Loc MBInactive -- all children of inactive region are inactive
                             }
                | MBIStale   { mbiRegion :: Either Region String
                             , mbiEpoch  :: Int
                             } 
                deriving (Show)

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
             deriving (Show)

isMBActive :: MBDescr -> Bool
isMBActive (MBA _) = True
isMBActive _       = False

mbEpoch :: MBDescr -> Int
mbEpoch (MBA mba) = mbaEpoch mba
mbEpoch (MBI mbi) = mbiEpoch mbi

data MBID    = MBID Pos [Loc] deriving (Eq, Ord, Show)
--data MBEpoch = MBEpoch Int [Int]

mbidChild :: MBID -> Loc -> MBID
mbidChild (MBID p ls) l = MBID p (ls ++ [l])

mbidParent :: MBID -> Maybe MBID
mbidParent (MBID _ [])  = Nothing
mbidParent (MBID p ls)  = Just $ MBID p $ init ls

mbidFile :: MBID -> FilePath
mbidFile (MBID p _) = sourceName $ fst p

isParentOf :: MBID -> MBID -> Bool
isParentOf (MBID p1 ls1) (MBID p2 ls2) = (p1 == p2) && length ls1 < length ls2 && isPrefixOf ls1 ls2

data CodeWin = CodeWin { cwAPI       :: CwAPI
                       , cwView      :: G.Widget
                       , cwPos       :: G.Widget
                       , cwFiles     :: M.Map SourceName (Region, G.TextTag, String) -- Source files and corresponding regions, tags, and last saved content
                       , cwMBRoots   :: M.Map Pos MBDescr                            -- Roots of the MB hierarchy
                       , cwActiveMB  :: Maybe MBID                                   -- Currently active MB
                       , cwSelection :: Maybe (Region, Pos, G.TextTag)
                       , cwCBEnabled :: Bool
                       }

cwLookupMB :: CodeWin -> MBID -> Maybe MBDescr
cwLookupMB cw (MBID p ls) = lookupMB (cwMBRoots cw M.! p) ls

lookupMB :: MBDescr -> [Loc] -> Maybe MBDescr
lookupMB mb        []                = Just mb
lookupMB (MBA mba)            (l:ls) = maybe Nothing (\mb'  -> lookupMB mb'        ls) $ M.lookup l (mbaNested mba)
lookupMB (MBI MBICurrent{..}) (l:ls) = maybe Nothing (\mbi' -> lookupMB (MBI mbi') ls) $ M.lookup l mbiNested
lookupMB (MBI _)              _      = Nothing

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
    Code cwAPI cwView cwPos <- codeWidgetNew "tsl" 600 600
    G.widgetShow cwView
    G.widgetShow cwPos

    ref <- newIORef $ error "codeWinNew: undefined"

    let files = nub 
                $ map (sourceName . fst)
                $ map pos specTemplate ++ map pos specType ++ map pos specConst ++ concatMap (map pos . F.tmProcess) specTemplate
    cwFiles <- trace ("codeWinNew: files=" ++ show files)
               $ liftM M.fromList 
               $ mapM (\n -> do reg <- pageCreate cwAPI n
                                regionEditable cwAPI reg False
                                tag <- tagNew cwAPI reg
                                txt <- regionGetAllText cwAPI reg
                                return (n, (reg, tag, txt))) files
    cwMBRoots <- liftM M.fromList
                 $ mapM (\p -> do let parent = sel1 $ cwFiles M.! (sourceName $ fst p)
                                  reg <- regionCreateFrom cwAPI parent p True (editCB ref $ MBID p [])
                                  return (p, MBI $ MBIStale (Left reg) 0)) 
                 $ specFindMBs spec
    writeIORef ref $ CodeWin{cwActiveMB = Nothing, cwSelection = Nothing, cwCBEnabled = True, ..}
    return ref

codeWinWidget :: RCodeWin -> IO G.Widget
codeWinWidget = getIORef cwView

codeWinPos    :: RCodeWin -> IO G.Widget
codeWinPos    = getIORef cwPos

codeWinActiveMB :: RCodeWin -> IO (Maybe MBID)
codeWinActiveMB = getIORef cwActiveMB

codeWinLookupMB :: RCodeWin -> MBID -> IO (Maybe MBDescr)
codeWinLookupMB ref mbid = getIORef (\cw -> cwLookupMB cw mbid) ref

codeWinGetMB :: RCodeWin -> MBID -> IO MBDescr
codeWinGetMB ref mbid = getIORef (\cw -> cwGetMB cw mbid) ref

codeWinModifiedFiles :: RCodeWin -> IO [String]
codeWinModifiedFiles ref = do
    CodeWin{..} <- readIORef ref
    liftM (map fst)
     $ filterM (\(_,(reg,_,txt)) -> do txt' <- regionGetAllText cwAPI reg
                                       return $ txt /= txt') 
     $ M.toList cwFiles

codeWinSaveAll :: RCodeWin -> IO ()
codeWinSaveAll ref = do
    CodeWin{..} <- readIORef ref
    tosave <- codeWinModifiedFiles ref
    _ <- mapM (\n -> do let (reg, t, _) = cwFiles M.! n 
                        txt <- regionGetAllText cwAPI reg
                        writeFile n txt
                        modifyIORef ref $ \cw -> cw {cwFiles = M.insert n (reg,t,txt) cwFiles})
         tosave
    return ()

-- Change text inside inactive (current or stale) MB.  The MB will become stale and its epoch will increase by 1
codeWinSetMBText :: RCodeWin -> MBID -> String -> IO ()
codeWinSetMBText ref mbid txt = do
    putStrLn $ "codeWinSetMBText " ++ show mbid ++ " \"" ++ txt ++ "\""
    cw@CodeWin{..} <- readIORef ref
    let MBI mbi = cwGetMB cw mbid
    reg' <- case mbiRegion mbi of
                 Left reg -> do regionSetTextSafe ref reg txt
                                return $ Left reg
                 Right _  -> return $ Right txt
    modifyIORef ref $ \cw' -> cwSetMB cw' mbid $ MBI $ MBIStale reg' (mbiEpoch mbi + 1)

-- Make stale MB active, creating nested MBs if necessary
-- Assumes: MBID refers to an existing stale MB whose parent is active.
codeWinMBRefresh :: RCodeWin -> MBID -> CFA -> IO ()
codeWinMBRefresh ref mbid cfa = do
    putStrLn $ "codeWinMBRefresh " ++ show mbid
    cw@CodeWin{..} <- readIORef ref
    let MBI MBIStale{mbiRegion = Left ireg, ..} = cwGetMB cw mbid
    nested <- mapM (\loc -> do let p = cfaGetMBPos cfa loc
                               txt' <- regionGetBoundedText cwAPI ireg p
                               return (loc, MBIStale (Right txt') 0))
              $ cfaFindMBs cfa
    txt <- regionGetText cwAPI ireg
    writeIORef ref $ cwSetMB cw mbid $ MBI $ MBICurrent (Left ireg) mbiEpoch txt cfa (M.fromList nested)
    activate ref mbid
    
-- Activate MB
-- Assumes: MBID refer to an existing non-stale MB
codeWinMBActivate :: RCodeWin -> Maybe MBID -> IO ()
codeWinMBActivate ref Nothing                   = do
    CodeWin{..} <- readIORef ref
    maybe (return ()) 
          (\(MBID p _) -> deactivateRec ref (MBID p []))
          cwActiveMB
codeWinMBActivate ref (Just mbid@(MBID p locs)) = do
    putStrLn $ "codeWinMBActivate " ++ show mbid
    CodeWin{..} <- readIORef ref
    let Just mbid'@(MBID _ locs') = cwActiveMB
    (if' (Just mbid == cwActiveMB)                      (return ())
     $ if' (isJust cwActiveMB && isParentOf mbid mbid') (deactivateRec ref (MBID p (take (length locs + 1) locs'))) 
     $ if' (isJust cwActiveMB && isParentOf mbid' mbid) (do _ <- mapM (\ls -> activate ref (MBID p ls)) $ drop (length locs' + 1) $ inits locs
                                                            return ())
     $ do codeWinMBActivate ref Nothing
          _ <- mapM (\ls -> activate ref (MBID p ls)) $ inits locs
          return ())

-- Select range within currently active region
codeWinSetSelection :: RCodeWin -> Maybe MBID -> Pos -> String -> IO ()
codeWinSetSelection ref mmbid p color = do
    putStrLn $ "codeWinSetSelection: " ++ show mmbid ++ " " ++ show p 
    codeWinClearSelection ref
    cw@CodeWin{..} <- readIORef ref
    let (reg, tag) = maybe (let (r, t,_ ) = cwFiles M.! (sourceName $ fst p) in (r,t))
                           (\mbid@(MBID (p',_) _) -> let MBA mba = cwGetMB cw mbid 
                                                     in (mbaRegion mba, sel2 $ cwFiles M.! sourceName p'))
                           mmbid
    G.set tag [G.textTagBackground G.:= color]
    regionApplyTag cwAPI reg tag p
    regionScrollToPos cwAPI reg (fst p)
    writeIORef ref $ cw {cwSelection = Just (reg,p,tag)}

codeWinClearSelection :: RCodeWin -> IO ()
codeWinClearSelection ref = do
    CodeWin{..} <- readIORef ref
    maybe (return ())
          (\(reg, _, tag) -> regionRemoveTag cwAPI reg tag)
          cwSelection
    modifyIORef ref $ \cw -> cw{cwSelection = Nothing}

editCB :: RCodeWin -> MBID -> IO ()
editCB ref mbid = do
    cw@CodeWin{..} <- readIORef ref
    maybe (return ())
          (\(reg, p, tag) -> do regionRemoveTag cwAPI reg tag
                                regionApplyTag cwAPI reg tag p) 
          cwSelection
    when cwCBEnabled $ do let mmbi = cwLookupMB cw mbid
                          case mmbi of 
                               Just (MBI MBICurrent{..}) -> writeIORef ref $ cwSetMB cw mbid $ MBI $ MBIStale mbiRegion (mbiEpoch + 1)
                               _                         -> return ()



----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

regionSetTextSafe :: RCodeWin -> Region -> String -> IO ()
regionSetTextSafe ref reg str = do
    modifyIORef ref (\cw -> cw{cwCBEnabled = False})
    cw' <- readIORef ref
    regionSetText (cwAPI cw') reg str
    modifyIORef ref (\cw -> cw{cwCBEnabled = True})

regionDeleteSafe :: RCodeWin -> Region -> IO ()
regionDeleteSafe ref reg = do
    CodeWin{..} <- readIORef ref
    case cwSelection of 
         Just (reg', _, _) -> when (reg' == reg) $ codeWinClearSelection ref
         _                 -> return ()
    regionDelete cwAPI reg

--cwPosToRegion :: CodeWin -> Pos -> Region
--cwPosToRegion cw@CodeWin{..} pos = 
--    maybe (cwFiles M.! (sourceName $ fst pos))
--          (\mbid -> let MBA mba = cwGetMB cw mbid in mbaRegion mba)
--          cwActive

-- deactivate active MB and, recursively, its children
deactivateRec :: RCodeWin -> MBID -> IO ()
deactivateRec ref mbid = do
   putStrLn $ "deactivateRec " ++ show mbid
   cw <- readIORef ref
   _ <- mapM (deactivateRec ref) 
        $ filter (\mbid' -> isMBActive $ cwGetMB cw mbid')
        $ cwMBChildren cw mbid
   deactivate ref mbid

-- deactivate active MB; all children are assumed to be inactive
deactivate :: RCodeWin -> MBID -> IO ()
deactivate ref mbid = do
    putStrLn $ "deactivate " ++ show mbid
    cw@CodeWin{..} <- readIORef ref
    let MBA (MBActive{..}) = cwGetMB cw mbid
    -- Delete nested regions and merge text into the parent
    alltext <- regionGetAllText cwAPI mbaRegion
    nested <- mapM (\(loc, MBI mbi) -> do let Left reg = mbiRegion mbi
                                          text' <- regionGetAllText cwAPI reg
                                          regionDeleteSafe ref reg
                                          return $ (loc, mbi {mbiRegion = Right text'}))
              $ M.toList mbaNested
    regionSetTextSafe ref mbaRegion alltext
    regionEditable cwAPI mbaRegion True
    let mb' = MBI $ MBICurrent (Left mbaRegion) mbaEpoch mbaText mbaCFA (M.fromList nested)
    modifyIORef ref $ \cw' -> (cwSetMB cw' mbid mb') {cwActiveMB = mbidParent mbid}
                   

-- activate inactive MB whose parent is already active
activate :: RCodeWin -> MBID -> IO ()
activate ref mbid = do
    putStrLn $ "activate " ++ show mbid
    cw@CodeWin{..} <- readIORef ref
    let mb = cwGetMB cw mbid
    let MBI MBICurrent{mbiRegion=Left reg, ..} = mb
    regionSetTextSafe ref reg mbiText
    regionEditable cwAPI reg False
    -- Create and populate nested regions
    nested <- mapM (\(loc, mbi) -> do let Right content = mbiRegion mbi
                                      reg' <- regionCreateFrom cwAPI reg (cfaGetMBPos mbiCFA loc) True (editCB ref $ mbidChild mbid loc)
                                      regionSetTextSafe ref reg' content
                                      return (loc, MBI $ mbi{mbiRegion = Left reg'}))
              $ M.toList mbiNested
    writeIORef ref $ (\cw' -> cw' {cwActiveMB = Just mbid})
                   $ cwSetMB cw mbid 
                   $ MBA $ MBActive reg mbiEpoch mbiText mbiCFA (M.fromList nested)

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
statFindMBs (F.SMagic   p)         = [p]
statFindMBs _                      = []


cfaGetMBPos :: CFA -> Loc -> Pos
cfaGetMBPos cfa l = p
    where ActStat (F.SMagic p) = locAct $ cfaLocLabel l cfa

cfaFindMBs :: CFA -> [Loc]
cfaFindMBs cfa = nub 
                 $ filter (\l -> case locAct $ cfaLocLabel l cfa of
                                      ActStat (F.SMagic _) -> True
                                      _                    -> False)
                 $ cfaDelayLocs cfa
