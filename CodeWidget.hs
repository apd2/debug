module CodeWidget (codeWidgetNew,
                   Code(..), 
                   CwAPI(..),
                   Region, 
                   rootRegion, 
                   CwView, 
                   CwIter ) where

{--
design considerations:

1. The way the user interacts with the debugger is going to change many times as we 
experiment with it and, later, increase the level of automation.  Ideally, the 
source view widget interface should be general enough to accommodate these changes.  
Hence I believe that the interface should be defined in terms of nested text regions, 
rather than magic blocks and other synthesis-specific concepts.

2. Functions exported by the widget will take text coordinates.  These coordinates should be 
relative to the current region and should not be sensitive to changes in nested regions, e.g.:

BEGIN_STATIC_REGION
text
text
{nested_editable_region}
highlighted_text
END_STATIC_REGION

In this example, when the user adds some text to the editable region, coordinates of the highlighted region are returned as if the editable region was empty, i.e., had zero width and height.
--}


import qualified Graphics.UI.Gtk            as G
import qualified Graphics.UI.Gtk.SourceView as G
import Text.Parsec
import Text.Parsec.Pos
import Data.List
--import Data.Maybe
import Data.IORef
import Control.Monad
import Util


--------------------------------------------------------------
-- Constants
--------------------------------------------------------------
fontSrc    = "Courier 10 Pitch"
--fontSrc    = "Monospace 9 Pitch"


--------------------------------------------------------------
-- Types
--------------------------------------------------------------
--encapsulate types from gtk
type CwView  = G.SourceView
type CwIter  = G.TextIter

-- Region ID
type Region = Int

-- Root region is created automatically with the widget and cannot be destroyed
rootRegion :: Region
rootRegion = 0

-- noRegion is the root region's parent
noRegion :: Region
noRegion = -1


-- Interface of the widget
data CwAPI = CwAPI {
    regionCreate    :: Region                    -- non-editable parent region
                    -> SourcePos                 -- location inside parent region
                    -> Bool                      -- editable?
                    -> String                    -- initial text
                    -> IO Region,

    regionDelete    :: Region -> IO (),          -- delete specified region. NOTE: cannot delete rootRegion

    regionGetText   :: Region -> IO String,     
    regionSetText   :: Region -> String -> IO (),
    
    tagNew          :: IO G.TextTag,            -- allocate new tag.  Normal GTK functions can be used to configure associated FG and BG colors
    regionApplyTag  :: Region -> G.TextTag -> (SourcePos, SourcePos) -> IO (), -- Apply tag to coordinates within the region
    regionRemoveTag :: Region -> G.TextTag -> IO (),
    regionSetMark   :: Region -> G.TextMark -> SourcePos -> IO (),
    regionGetIter   :: Region -> SourcePos -> IO G.TextIter
}

-- RegionContext - contextual info for each region
data RegionContext = RegionContext {
      rcRegion    :: Region,
      rcParent    :: Region,
      rcEditable  :: Bool,
      rcStart     :: G.TextMark,
      rcEnd       :: G.TextMark,
      rcStartPos  :: SourcePos
}

instance Eq RegionContext where
    (==) (RegionContext _ p1 _ _ _  sp1 ) (RegionContext _ p2 _ _ _ sp2)  =  if' (p1 == p2) (sp1 == sp2)  False

instance Ord RegionContext where
    compare (RegionContext _ p1 _ _ _ sp1) (RegionContext _ p2 _ _ _ sp2) = if' (p1 == p2) (compare sp1 sp2) (compare p1 p2)

-- CodeView - GTK TextView/SourceView context to support Widget API
data CodeView =  CodeView {
    cvView        :: G.SourceView,
    cvBuffer      :: G.SourceBuffer,
    cvEditTag     :: G.TextTag,
    cvTagTable    :: G.TextTagTable,
    cvNextRegion  :: Region,
    cvRegions     :: [RegionContext],
    cvFileName    :: String
}

-- Code - tupple containing a CwAPI and a CwView, api and view for a CodeWidget Instance
data Code = Code {
    api     :: CwAPI,
    view    :: CwView
}

-- wrapped version
type RCodeView = IORef CodeView

-- API creation

{-- codeWidgetNew - create an instance of the code widget.
              codeWidgetNew :: String       -- language type (file extension)
                            -> Maybe String -- file name (optional)
                            -> Int       -- width
                            -> Int       -- height
                            -> IO Code
--}
codeWidgetNew :: String -> Maybe String -> Int -> Int -> IO Code
codeWidgetNew l f w h = do
    slm <- G.sourceLanguageManagerGetDefault
    mlng <- G.sourceLanguageManagerGetLanguage slm l
    lng <- case mlng of
              Nothing -> error $ "Can't find " ++ l ++ " Language Definition"
              Just x  -> return x

    table <- G.textTagTableNew
    buf <- G.sourceBufferNew (Just table)
    etag <- G.textTagNew Nothing
    G.set etag [G.textTagEditable G.:= False]
    G.textTagTableAdd table etag

    G.sourceBufferSetLanguage buf (Just lng)
    G.sourceBufferSetHighlightSyntax buf True
    v <- G.sourceViewNewWithBuffer buf
    font <- G.fontDescriptionFromString fontSrc
    
    G.widgetModifyFont v $ Just font
    G.widgetSetSizeRequest v w h
    G.textViewSetEditable v True
    root <- mkRootRegion buf
    ref <- newIORef $ CodeView { cvView       = v
                               , cvBuffer     = buf
                               , cvTagTable   = table
                               , cvEditTag    = etag
                               , cvNextRegion = (rootRegion + 1)
                               , cvRegions    = [root]
                               , cvFileName   = case f of
                                                     Nothing -> ""
                                                     Just s  -> s
                               }

    return $ Code { api = CwAPI { regionCreate     = codeRegionCreate    ref
                                , regionDelete     = codeRegionDelete    ref
                                , regionGetText    = codeRegionGetText   ref
                                , regionSetText    = codeRegionSetText   ref
                                , tagNew           = codeTagNew          ref
                                , regionApplyTag   = codeRegionApplyTag  ref
                                , regionRemoveTag  = codeRegionRemoveTag ref
                                , regionSetMark    = codeRegionSetMark   ref
                                , regionGetIter    = codeRegionGetIter   ref
                                }
                  , view = v
                  }

-- Individual API functions

codeRegionCreate :: RCodeView -> Region -> SourcePos -> Bool -> String -> IO Region
codeRegionCreate ref parent pos ed txt = do
    cv <- readIORef ref
    case cvGetRegion cv parent of
          Nothing -> error ("regionCreate: cannot find parent region " ++ (show parent))
          Just r  -> do let rc = cvNextRegion cv
                        let currgns = cvRegions cv
                        smk <- newLeftMark
                        emk <- newRightMark
                        rpos <- cvRgnMapPos cv r pos
                        iter <- rootIterFromPos cv rpos
                        G.textBufferAddMark (cvBuffer cv) smk iter
                        G.textBufferAddMark (cvBuffer cv) emk iter
                        let newrgn = RegionContext { rcRegion   = rc
                                                   , rcParent   = parent
                                                   , rcEditable = ed
                                                   , rcStart    = smk
                                                   , rcEnd      = emk
                                                   , rcStartPos = rpos
                                                   }
                        writeIORef ref cv { cvNextRegion =  rc + 1, cvRegions = newrgn:currgns }
                        codeRegionSetText ref rc txt
                        return rc
                      
codeRegionDelete :: RCodeView -> Region -> IO ()
codeRegionDelete ref r = do
    sv <- readIORef ref
    if r > 0 
        then case cvGetRegion sv r of 
                  Nothing -> error ("regionDelete: specified region does not exist: " ++ (show r))
                  Just x  -> do let newrgns = cvOtherRegions sv r
                                G.textBufferDeleteMark (cvBuffer sv) (rcStart x)
                                G.textBufferDeleteMark (cvBuffer sv) (rcEnd x)
                                writeIORef ref sv {cvRegions = newrgns}
        else if' (r == 0) (error "regionDelete: attempt to delete root region!") (error $ "regionDelete: invalid negative region " ++ (show r))


codeRegionGetText :: RCodeView -> Region -> IO String
codeRegionGetText ref r = do
    cv <- readIORef ref
    case cvGetRegion cv r of 
            Nothing -> error ("cvRegionGetText: region not found: " ++ (show r))
            Just x  -> cvSubRgnText cv x


codeRegionSetText :: RCodeView -> Region -> String -> IO ()
codeRegionSetText ref r txt = do
    cv <- readIORef ref
    case cvGetRegion cv r of 
            Nothing -> error ("cvRegionGetText: region not found: " ++ (show r))
            Just x -> if (cvIsRoot x) 
                          then do G.textBufferSetText (cvBuffer cv) txt
                                  
                          else do iter1 <- G.textBufferGetIterAtMark (cvBuffer cv) (rcStart x)
                                  iter2 <- G.textBufferGetIterAtMark (cvBuffer cv) (rcEnd x)
                                  G.textBufferDelete (cvBuffer cv) iter1 iter2
                                  G.textBufferInsert (cvBuffer cv) iter1 txt
    cvSetEditFlags cv 

codeTagNew :: RCodeView -> IO G.TextTag
codeTagNew ref = do 
    cv  <- readIORef ref
    tag <- G.textTagNew Nothing
    G.textTagTableAdd (cvTagTable cv) tag
    return tag


codeRegionApplyTag :: RCodeView -> Int -> G.TextTag -> (SourcePos, SourcePos) -> IO ()
codeRegionApplyTag ref r t (from, to) = do
    cv <- readIORef ref
    case cvGetRegion cv r of 
            Nothing -> error ("regionApplyTag: region not found: " ++ (show r))
            Just x -> do rfrom <- cvRgnMapPos cv x from
                         rto   <- cvRgnMapPos cv x to
                         siter <- rootIterFromPos cv rfrom
                         eiter <- rootIterFromPos cv rto
                         G.textBufferApplyTag (cvBuffer cv) t siter eiter


codeRegionRemoveTag :: RCodeView -> Int -> G.TextTag -> IO ()
codeRegionRemoveTag ref r t = do
    cv <- readIORef ref
    case cvGetRegion cv r of 
            Nothing -> error ("regionRemoveTag: region not found: " ++ (show r))
            Just x -> do iter1 <- cvRgnStart cv x
                         iter2 <- cvRgnEnd cv x
                         G.textBufferRemoveTag (cvBuffer cv) t iter1 iter2


codeRegionSetMark :: RCodeView -> Region -> G.TextMark -> SourcePos -> IO ()
codeRegionSetMark ref r m p = do
    cv <- readIORef ref
    case cvGetRegion cv r of
            Nothing -> error ("regionSetMark: region not found: " ++ (show r))
            Just x -> do rpos <- cvRgnMapPos cv x p
                         apos <- cvAllowForPriorSubs cv x rpos
                         iter <- G.textBufferGetIterAtLineOffset (cvBuffer cv) (sourceLine apos - 1) (sourceColumn apos - 1) 
                         G.textBufferAddMark (cvBuffer cv) m iter

codeRegionGetIter :: RCodeView -> Region -> SourcePos -> IO G.TextIter
codeRegionGetIter ref r p = do
    cv <- readIORef ref
    case cvGetRegion cv r of
            Nothing -> error ("regionGetIter: region not found: " ++ (show r))
            Just x -> do rpos <- cvRgnMapPos cv x p
                         apos <- cvAllowForPriorSubs cv x rpos
                         G.textBufferGetIterAtLineOffset (cvBuffer cv) (sourceLine apos - 1) (sourceColumn apos - 1) 

-- Helper functions

-- get a list of all regions except the specified one
cvOtherRegions :: CodeView -> Region -> [RegionContext]
cvOtherRegions cv r = filter (\x -> r /= (rcRegion x)) $ cvRegions cv

-- get the specified region, maybe
cvGetRegion :: CodeView -> Region -> Maybe RegionContext
cvGetRegion cv r = 
    let rgs = filter (\x -> r == (rcRegion x)) $ cvRegions cv
    in case rgs of 
            [] -> Nothing
            _  -> Just $ head rgs

cvSetEditFlags :: CodeView -> IO ()
cvSetEditFlags cv = do
    case cvGetRegion cv rootRegion of
         Nothing -> error "no root region"
         Just r  -> cvRgnStatic cv r
    mapM_ (cvRgnEditable cv) (cvEditableRgns cv)

cvEditableRgns :: CodeView -> [RegionContext]
cvEditableRgns cv = filter rcEditable (cvRegions cv)

cvRgnStatic :: CodeView -> RegionContext -> IO ()
cvRgnStatic cv rc = do
    si <- cvRgnStart cv rc
    ei <- cvRgnEnd   cv rc
    let buf = cvBuffer cv
    let tag = cvEditTag cv
    G.textBufferRemoveTag buf tag si ei
    G.textBufferApplyTag buf tag si ei 

cvRgnEditable :: CodeView -> RegionContext -> IO ()
cvRgnEditable cv rc = do
    si <- cvRgnStart cv rc
    ei <- cvRgnEnd   cv rc
    G.textBufferRemoveTag (cvBuffer cv) (cvEditTag cv) si ei

-- Get a TextIter set to the start of the region
cvRgnStart :: CodeView -> RegionContext -> IO G.TextIter
cvRgnStart cv rc = do
    G.textBufferGetIterAtMark (cvBuffer cv) (rcStart rc)

-- Get a TextIter set to the end of the region
cvRgnEnd :: CodeView -> RegionContext -> IO G.TextIter
cvRgnEnd cv rc = do
    G.textBufferGetIterAtMark ( cvBuffer cv) (rcEnd rc)

-- convert a TextIter into a SourcePos
posFromIter :: CodeView -> G.TextIter -> IO SourcePos
posFromIter cv iter = do
    l <- G.textIterGetLine iter
    c <- G.textIterGetLineOffset iter
    return $ newPos (cvFileName cv) (l + 1)  (c + 1)
    
-- get region's current starting SourcePos - 
cvRgnStartPos :: CodeView -> RegionContext -> IO SourcePos
cvRgnStartPos cv rc = do
    iter <- cvRgnStart cv rc
    posFromIter cv iter

-- get region's initial position - may be different from current starting SourcePos due to edits to preceeding regions
cvRgnInitPos :: CodeView -> RegionContext -> SourcePos
cvRgnInitPos _ rc = rcStartPos rc

-- get region's initial line
cvRgnInitLine cv rc = sourceLine (cvRgnInitPos cv rc)

-- get region's ending SourcePos
cvRgnEndPos :: CodeView -> RegionContext -> IO SourcePos
cvRgnEndPos cv rc = do
    iter <- cvRgnEnd cv rc
    posFromIter cv iter

-- map a Region.SourcePos to a global buffer position - makes adjustments for other subregions
cvRgnMapPos :: CodeView -> RegionContext -> SourcePos -> IO SourcePos
cvRgnMapPos cv rc p = do
    p2 <- cvRgnStartPos cv rc
    let nl = (sourceLine p) + (sourceLine p2) - 1
    let nc = (sourceColumn p) + (sourceColumn p2) - 1
    let np = newPos (sourceName p) nl nc
    cvAllowForPriorSubs cv rc np

-- return the number of lines in a region
cvRgnHeight :: CodeView -> RegionContext -> IO Line
cvRgnHeight cv rc = do
    spos <- cvRgnStartPos cv rc
    epos <- cvRgnEndPos cv rc
    return $ (sourceLine epos) - (sourceLine spos)

-- return the width of a region - the difference bewteen the region's position at creating and its current ending position column
cvRgnWidth :: CodeView -> RegionContext -> IO Column
cvRgnWidth cv rc = do
    spos <- cvRgnStartPos cv rc
    epos <- cvRgnEndPos cv rc
    return $ (sourceColumn epos) - (sourceColumn spos)

-- Get a root-normalized TextIter for the given position
rootIterFromPos :: CodeView -> SourcePos -> IO G.TextIter
rootIterFromPos cv pos = do
    G.textBufferGetIterAtLineOffset (cvBuffer cv)  (sourceLine pos - 1)  (sourceColumn pos - 1)

-- Adjust a region position to account for a preceeding subregion
cvAdjustForSub :: CodeView -> SourcePos -> RegionContext -> IO SourcePos
cvAdjustForSub cv pos rc = do
    let ln = cvRgnInitLine cv rc
    let fn = sourceName pos
    if sourceLine pos > ln
        then do h <- cvRgnHeight cv rc
                let nln = (sourceLine pos) + h
                let np =  newPos fn nln (sourceColumn pos)
                return np
        else if sourceLine pos == ln
                then do w <- cvRgnWidth cv rc
                        h <- cvRgnHeight cv rc
                        let ncol = (sourceColumn pos) + w
                        let np =  newPos fn (h + (sourceLine pos)) ncol
                        return np
                else do return pos
            

-- Adjust a SourcePos for any edits done to editable sub-regions
cvAllowForPriorSubs :: CodeView -> RegionContext -> SourcePos -> IO SourcePos
cvAllowForPriorSubs cv rc p = do
    np <- foldM (\rgn -> cvAdjustForSub cv rgn) p (cvOtherRegions cv (rcRegion rc))
    return np

-- is specified region the rootRegion?
cvIsRoot :: RegionContext -> Bool
cvIsRoot rc = if' (rcRegion rc == rootRegion) True False

-- create a new left-side mark
newLeftMark :: IO G.TextMark
newLeftMark = do mk <- G.textMarkNew Nothing True
                 G.textMarkSetVisible mk False
                 return mk

-- create a new right-side mark
newRightMark :: IO G.TextMark
newRightMark = do mk <- G.textMarkNew Nothing False
                  G.textMarkSetVisible mk False
                  return mk

-- create the initial root region
mkRootRegion :: G.SourceBuffer -> IO RegionContext
mkRootRegion bf = do smk <- newLeftMark
                     emk <- newRightMark
                     i1 <- G.textBufferGetStartIter bf
                     i2 <- G.textBufferGetStartIter bf
                     G.textBufferAddMark bf smk i1 
                     G.textBufferAddMark bf emk i2 
                     let pos = newPos "" 1 1
                     let r =  RegionContext  { rcRegion   = rootRegion
                                             , rcParent   = noRegion
                                             , rcEditable = False
                                             , rcStart    = smk
                                             , rcEnd      = emk 
                                             , rcStartPos = pos}
                     return r

-- wrapper around G.textBufferGetText - debugging aid
cvRgnGetText :: CodeView -> G.TextIter -> G.TextIter -> Bool -> IO String
cvRgnGetText cv es ee b = do
      --spos <- posFromIter cv es
      --epos <- posFromIter cv ee
      --putStrLn $ "GET TEXT - S:" ++ (show spos) ++ " E:" ++ (show epos)
      G.textBufferGetText (cvBuffer cv) es ee b

-- Get a list of regions which are nested in the specified region
cvSubRegions :: CodeView -> RegionContext -> [RegionContext]
cvSubRegions cv rc = sort $ filter (\x -> (rcParent x) == (rcRegion rc)) (cvRegions cv)

-- Build a string from the gaps between the regions in the list
cvSubRgnGapText :: CodeView -> [RegionContext] -> IO String
cvSubRgnGapText _  []  = do return ""
cvSubRgnGapText _  (_:[]) = do  return ""
cvSubRgnGapText cv (x:xs) = do  es <- cvRgnEnd cv x 
                                let x2 = head xs
                                ee <- cvRgnStart cv x2
                                s1 <- cvRgnGetText cv es ee False
                                s2 <- cvSubRgnGapText cv xs
                                return $ s1 ++ s2

-- Get the text for this region, ignoring text in any sub-regions
cvSubRgnText :: CodeView -> RegionContext -> IO String
cvSubRgnText cv rc = do 
          let sr = cvSubRegions cv rc
          case sr of 
               []       -> do es <- (cvRgnStart cv rc)
                              ee <- cvRgnEnd cv rc
                              cvRgnGetText cv es ee False
               (x:[])   -> do es1 <- cvRgnStart cv rc
                              ee1 <- cvRgnStart cv x  
                              es3 <- cvRgnEnd cv x
                              ee3 <- cvRgnEnd cv rc
                              s1 <- cvRgnGetText cv es1 ee1 False
                              s3 <- cvRgnGetText cv es3 ee3 False
                              return $ s1 ++ s3
               (x:xs)   -> do es1 <- cvRgnStart cv rc
                              ee1 <- cvRgnStart cv x
                              let x2 = last xs
                              es3 <- cvRgnEnd cv x2
                              ee3 <- cvRgnEnd cv rc
                              s1 <- cvRgnGetText cv es1 ee1 False
                              s2 <- cvSubRgnGapText cv (x:xs)
                              s3 <- cvRgnGetText cv es3 ee3 False
                              return $ s1 ++ s2 ++ s3

