{-# LANGUAGE RecordWildCards #-}

module CodeWin(CodeWin,
               codeWinNew) where

import qualified Data.Map        as M
import qualified Graphics.UI.GTK as G
import Data.IORef

import Util
import Pos
import qualified Spec            as F
import qualified Template        as F
import qualified Method          as F
import qualified Statement       as F
import CodeWidget

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

data MBDescr = MBA MBActive
             | MBI MBInactive

isMBActive :: MBDescr -> Bool
isMBActive (MBA _) = True
isMBActive _       = False

data MBID    = MBID Pos [Loc]
data MBEpoch = MBEpoch Int [Int]

mbidChild :: MBID -> Loc -> MBID

data CodeWin = CodeWin { cwAPI       :: CwAPI
                       , cwView      :: CwView
                       , cwScroll    :: G.ScrolledWindow                     -- top-level widget
                       , cwFiles     :: M.Map SourceName (Region, G.TextTag) -- Source files and corresponding regions and tags
                       , cwMBRoots   :: M.Map Pos MBDescr                    -- Roots of the MB hierarchy
                       , cwActiveMB  :: Maybe MBID                           -- Currently active MB
                       , cwSelection :: Maybe (Region, G.TextTag)
                       }

cwLookupMB :: CodeWin -> MBID -> Maybe MBDescr

cwGetMB :: CodeWin -> MBID -> MBDescr

cwMBChildren :: CodeWin -> MBID -> MBID

cwSetMB :: CodeWind -> MBID -> MBDescr -> CodeWin

type RCodeWin = IORef CodeWin

-- Load all spec files and initialise cwMBRoots
codeWinNew :: F.Spec -> IO RCodeWin
codeWinNew spec@Spec{..} = do
    -- Source window at the top
    cwScroll <- G.scrolledWindowNew Nothing Nothing
    G.widgetShow scroll
    Code cwAPI cwView <- C.codeWidgetNew "tsl" 600 600
    G.containerAdd cwScroll (view cwCode)

    ref <- newIORef $ error "codeWinNew: undefined"

    files = nub $ map (G.sourceName . fst)
                $ map pos specTemplate ++ map pos specType ++ map pos specConst
    cwFiles <- liftM M.fromList 
               $ mapM (\n -> do reg <- createRootRegion (api code) n
                                tag <- regionNewTag cwAPI reg
                                return (n, (reg, tag))) files
    cwMBRoots <- liftM M.fromList
                 $ mapM (\pos -> do let parent = fst $ cwFiles M.! G.sourceName $ fst pos
                                    reg <- regionCreateFrom cwAPI parent pos True (editCB ref $ MBID pos [])
                                    return (pos, MBI $ MBIStale (Just reg) 0)) 
                 $ specFindMBs spec
    writeIORef ref $ CodeWin{cwActiveMB = Nothing, cwSelection = Nothing, ..}

codeWinWidget :: RCodeWin -> IO G.Widget
codeWinWidget = getIORef cwScroll

-- Return epochs for all MBs along the path that still exist
codeWinMBEpoch :: RCodeWin -> MBID -> MBEpoch

-- Make stale MB active, creating nested MBs if necessary
-- Assumes: MBID refers to an existing stale MB whose parent is active.
codeWinMBRefresh :: RCodeWin -> MBID -> CFA -> IO ()
codeWinMBRefresh ref mbid cfa = do
    cw@CodeWin{..} <- readIORef ref
    let MBI MBIStale{..} = getMB cw mbid
    regionMakeNonEditable cwAPI $ fromJust mbsRegion
    nested <- mapM (\loc -> do let pos = cfaGetMBPos loc
                               reg <- regionCreateFrom cwAPI mbsRegion pos True (editCB ref $ mbidChild mbid loc)
                               return (loc, MBI $ MBIStale (Just reg) 0))
              $ cfaFindMBs cfa
    mb' = MBA $ MBActive mbsRegion mbsEpoch cfa (M.fromList nested)
    writeIORef ref $ cwSetMB cw mbid mb'

-- Deactivate all active MBs
codeWinMBDeactivate :: RCodeWin -> IO ()
codeWinMBDeactivate ref = do
    CodeWin{..} <- readIORef
    maybe (return ()) 
          (\(MBID pos _) -> deactivateRec ref (MBID pos []))
          cwActiveMB

-- Activate MB
-- Assumes: MBID refer to an existing non-stale MB
codeWinMBActivate :: RCodeWin -> MBID -> IO ()
codeWinMBActivate ref mbid@(MBID pos locs) = do
    let MBID pos' locs' = cwActiveMB
    CodeWin{..} <- readIORef ref
    if' (mbid == cwActiveMB)           (return ())
    $ if' (isParentOf mbid cwActiveMB) (deactivateRec ref (MBID pos (take (length locs + 1) locs')))
    $ if' (isParentOf cwActiveMB mbid) do mapM (\ls -> activate ref (MBID pos ls)) $ drop (length locs') $ inits locs
                                          return ()
    $ do codeWinMBDeactivate ref
         mapM (\ls -> activate ref (MBID pos ls)) $ inits locs
         return ()

-- Select range with currently active region
codeWinSetSelection :: RCodeWin -> Pos -> String -> IO ()
codeWinSetSelection ref pos color = do
    codeWinClearSelection ref
    cw@CodeWin{..} <- readIORef ref
    let (reg, tag) = maybe (cwFiles M.! (sourceName $ fst pos))
                           (\mbid@(MBID (p,_) _) -> let MBA mba = cwGetMB cw mbid 
                                                    in (mbaRegion mba, snd $ cwFiles M.! sourceName p))
                           cwActive
    G.set cwSelTag [G.textTagBackground G.:= color]
    regionApplyTag cwAPI reg cwSelTag pos
    writeIORef ref $ cw {cwSelTag = Just (reg,tag)}

codeWinClearSelection :: RCodeWin -> IO ()
codeWinClearSelection ref =
    cw@CodeWin{..} <- readIORef ref
    maybe (return ())
          (\(reg, tag) -> regionRemoveTag cwAPI reg tag)
          cwSelection

editCB :: RCodeWin -> MBID -> IO ()
editCB ref mbid = do
    cw <- readIORef ref
    let MBI mbi = cwGetMB cw mbid
    case mbi of 
         MBIStale{..}   -> return ()
         MBICurrent{..} -> writeIORef ref $ cwSetMB cw $ MBI $ MBIStale mbiRegion (mbiEpoch + 1)

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
   mapM (deactivateRec ref) 
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
    regionMakeEditable cwAPI reg
    let mb' = MBI $ MBICurrent (Left mbaRegion) mbaEpoch text mbaCFA (M.fromList nested)
    writeIORef ref $ cwSetMB cw mbid mb'

-- activate inactive MB whose parent is already active
activate :: RCodeWin -> MBID -> IO ()
activate ref mbid = do
    cw@CodeWin{..} <- readIORef ref
    let MBI MBICurrent{mbiRegion=Left reg, ..} = cwGetMB cw mbid
    regionSetText cwAPI reg mbiText
    regionMakeNonEditable cwAPI reg
    -- Create and populate nested regions
    nested <- mapM (\(loc, mbi) -> do let Right content = mbiRegion mbi
                                      reg' <- regionCreate cwAPI reg (cfaGetMBPos mbiCFA loc) True content (editCB ref $ mbidChild mbid loc)
                                      return (loc, MBI $ mbi{mbiRegion = Left reg'}))
              $ M.toList mbiNested
    writeIORef ref $ cwSetMB cw mbid 
                   $ MBActive reg mbiEpoch mbiCFA (M.fromList nested)

-- Listing all MBs in a spec
specFindMBs :: F.Spec -> [Pos]
specFindMBs spec = concatMap tmFindMBs $ specTemplate spec
 
tmFindMBs :: F.Template -> [Pos]
tmFindMBs tm = concatMap $ map (statFindMBs . procStatement) (tmProcess tm) ++ map methFindMBs (tmMethod tm)

methFindMBs :: F.Method -> [Pos]
methFindMBs meth = case methBody meth of
                        Left (mb, ma) -> concatMap statFindMBs $ catMaybes [mb, ma]
                        Right s       -> statFindMBs s

statFindMBs :: F.Statement -> [Pos]
statFindMBs (SSeq     _ ss)      = concatMap statFindMBs ss
statFindMBs (SPar     _ ps)      = concatMap (statFindMBs . snd) ss
statFindMBs (SForever _ b)       = statFindMBs b
statFindMBs (SDo      _ b _)     = statFindMBs b
statFindMBs (SWhile   _ _ b)     = statFindMBs b
statFindMBs (SFor     _ _ b)     = statFindMBs b
statFindMBs (SChoice  _ ss)      = concatMap statFindMBs ss
statFindMBs (SITE     _ _ t me)  = concatMap statFindMBs $ t : maybeToList me 
statFindMBs (SCase    _ _ cs md) = concatMap statFindMBs $ map snd cs ++ maybeToList md
statFindMBs (SMagic   _ p _)     = [p]
statFindMBs _                    = []
