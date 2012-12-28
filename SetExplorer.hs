{-# LANGUAGE RecordWildCards, ImplicitParams, ScopedTypeVariables #-}

module SetExplorer(RSetExplorer,
                   setExplorerNew,
                   setExplorerSetRelation,
                   setExplorerReset,
                   setExplorerGetVarAssignment,
                   setExplorerGetWidget,
                   SetExplorerEvents(..)) where

import Safe
import Data.IORef
import Data.Bits
import Data.Maybe
import Data.List
import Control.Monad
import qualified Data.Set        as S
import qualified Graphics.UI.Gtk as G

import Util
import qualified DbgTypes        as D
import Implicit

----------------------------------------------------------
-- Constants
----------------------------------------------------------

colorDisabled = G.Color 20000 20000 20000
colorChanged  = G.Color 65535 0     0
colorNormal   = G.Color 0     0     0

data SetExplorerEvents = SetExplorerEvents {
    evtValueChanged :: IO ()
}

----------------------------------------------------------
-- Types
----------------------------------------------------------

data SetExplorer c a = SetExplorer {
    seCtx            :: c,
    seCB             :: SetExplorerEvents,
    seRel            :: a,
    seVBox           :: G.VBox,
    seSpin           :: G.SpinButton,
    seStore          :: G.ListStore (VarEntry a),
    seCover          :: [a]          -- prime cover of seRel conjuncted with user selections
}

type RSetExplorer c a = IORef (SetExplorer c a)

data VarEntry a = VarEntry {
    varName              :: String,
    varType              :: D.Type,
    varIndices           :: [Int],
    varUserSelectionText :: String,
    varAssignment        :: a,        -- selected variable value(s)
    varEnabled           :: Bool,     -- False = don't care variable
    varChanged           :: Bool      -- True = highlight variable as changed
}

varVar :: (D.Rel c v a s, ?m::c) => VarEntry a -> v
varVar = D.idxToVS . varIndices

----------------------------------------------------------
-- External interface
----------------------------------------------------------

setExplorerNew :: D.Rel c v a s => c -> [(String, D.Type, [Int])] -> SetExplorerEvents -> IO (RSetExplorer c a)
setExplorerNew ctx vars cb = do
    let ?m = ctx
    -- vbox to hold explorer widgets
    vbox <- G.vBoxNew False 0 
    G.widgetShow vbox

    -- spin button
    adj <- G.adjustmentNew 0 0 0 1 1 1
    spin <- G.spinButtonNew adj 1 0
    G.spinButtonSetNumeric spin True
    G.spinButtonSetUpdatePolicy spin G.UpdateIfValid
    --G.spinButtonSetIncrements spin 1 (-1)

    -- don't show the spin button if there is only one variable
    if length vars > 1
       then do G.boxPackStart vbox spin G.PackNatural 0
               G.widgetShow spin
       else return ()

    -- list store
    let entries = map (\(n,d,i) -> VarEntry { varName              = n
                                            , varType              = d
                                            , varIndices           = i
                                            , varUserSelectionText = "*"
                                            , varAssignment        = b
                                            , varEnabled           = True
                                            , varChanged           = False })
                      vars
    store <- G.listStoreNew entries

    -- list view
    view <- G.treeViewNewWithModel store
    G.boxPackStart vbox view G.PackGrow 0
    G.widgetShow view

    let addColumn :: String -> [(G.Object,(G.TreeIter -> IO ()))] -> IO ()
        addColumn title renderers = do
            col <- G.treeViewColumnNew
            G.treeViewColumnSetTitle col title
            mapM (\(w,func) -> do let rend = G.castToCellRenderer w
                                  G.cellLayoutPackStart col rend False
                                  G.cellLayoutSetAttributeFunc col rend store func)
                 renderers
            G.treeViewAppendColumn view col
            return ()

    let nodeFromIter iter = do
            let idx = G.listStoreIterToIndex iter
            G.listStoreGetValue store idx

    -- Variable column
    varNameRend <- G.cellRendererTextNew
    let nameAttrFunc iter = do
            e@VarEntry{..} <- nodeFromIter iter 
            G.set varNameRend [G.cellText G.:= varName,
                               G.cellTextForegroundColor G.:= entryColor e]
    addColumn "Variable" [(G.toObject varNameRend,nameAttrFunc)]

    -- Type column
    typeRend <- G.cellRendererTextNew
    let typeAttrFunc iter = do
            e@VarEntry{..} <- nodeFromIter iter
            G.set typeRend [G.cellText G.:= show varType,
                            G.cellTextForegroundColor G.:= entryColor e]        
    addColumn "Type" [(G.toObject typeRend,typeAttrFunc)]

    -- Constraint
    constrTextRend  <- G.cellRendererTextNew
    constrComboRend <- G.cellRendererComboNew

    let textSetFunc iter = do 
            e@VarEntry{..} <- nodeFromIter iter    
            G.set constrTextRend [G.cellVisible G.:= case varType of
                                                          D.Bool   -> False
                                                          D.Enum _ -> False
                                                          D.SInt _ -> True
                                                          D.UInt _ -> True,
                                  G.cellTextEditable G.:= True,
                                  G.cellTextForegroundColor G.:= entryColor e,
                                  G.cellText G.:= varUserSelectionText]
                                  
        comboSetFunc iter = do
            e@VarEntry{..} <- nodeFromIter iter    
            G.set constrComboRend [G.cellVisible G.:= case varType of
                                                           D.Bool   -> True
                                                           D.Enum _ -> True
                                                           D.SInt _ -> False
                                                           D.UInt _ -> False,
                                   G.cellTextEditable G.:= True,
                                   G.cellComboHasEntry G.:= False,
                                   G.cellText G.:= varUserSelectionText]

    addColumn "Constraint" [(G.toObject constrTextRend, textSetFunc), (G.toObject constrComboRend, comboSetFunc)]

    -- Variable assignment column
    valRend <- G.cellRendererTextNew
    let valAttrFunc iter = do
            e <- nodeFromIter iter
            G.set valRend [G.cellText G.:= varAssignmentStr e,
                           G.cellTextForegroundColor G.:= entryColor e]        
    addColumn "Value" [(G.toObject valRend,valAttrFunc)]

    ref <- newIORef $ SetExplorer { seCtx   = ctx
                                  , seCB    = cb
                                  , seRel   = b
                                  , seVBox  = vbox
                                  , seSpin  = spin
                                  , seStore = store
                                  , seCover = []}

    G.on constrComboRend G.editingStarted (userConstraintSelectionStarted ref)
    G.on constrTextRend  G.edited         (userConstraintChanged ref)
    G.on constrComboRend G.edited         (userConstraintChanged ref) 
    G.afterValueSpinned spin (do idx <- (liftM round) $ G.get spin G.spinButtonValue
                                 showImplicant ref idx)

    G.treeViewSetHeadersVisible view True
    selection <- G.treeViewGetSelection view
    G.treeSelectionSetMode selection G.SelectionSingle

    return ref
    
setExplorerSetRelation :: (D.Rel c v a s) => RSetExplorer c a -> a -> IO ()
setExplorerSetRelation ref rel = do
    se <- readIORef ref
    let se' = se {seRel = rel}
    writeIORef ref se'
    entries <- G.listStoreToList $ seStore se'
    updateStore ref $ map varUserSelectionText entries

-- Clear all value selections
setExplorerReset :: (D.Rel c v a s) => RSetExplorer c a -> IO ()
setExplorerReset ref = updateStore ref $ repeat "*"

setExplorerGetVarAssignment :: RSetExplorer c a -> IO [(String, a)]
setExplorerGetVarAssignment ref = do
    se <- readIORef ref
    entries <- G.listStoreToList $ seStore se
    return $ map (\e -> (varName e, varAssignment e)) entries

setExplorerGetWidget :: RSetExplorer c a -> IO G.Widget
setExplorerGetWidget ref = (liftM $ G.toWidget . seVBox) $ readIORef ref

---------------------------------------------------------------------
-- GUI event handlers
---------------------------------------------------------------------

userConstraintSelectionStarted :: (D.Rel c v a s) => RSetExplorer c a -> G.Widget -> G.TreePath -> IO ()
userConstraintSelectionStarted ref w (idx:_) = do
    se@SetExplorer{..} <- readIORef ref
    let ?m = seCtx
    entries <- G.listStoreToList seStore
    let var@VarEntry{..} = entries !! idx
        combo = G.castToComboBox w
        -- user constraints over all other variables
        constrs = conj $ map varUserConstraint $ take idx entries ++ drop (idx+1) entries
        rel'    = constrs .& seRel
        -- partition variable values into available and unavailable values
        vals    = case varType of
                       D.Bool    -> ["false","true"]
                       D.Enum es -> es
                       _         -> []
        (avail, unavail) = partition ((/= b) . (rel' .&) . constraintFromStr var) vals
        sep = "====================="
    store <- G.listStoreNew $ ["*"] ++ [sep] ++ avail ++ [sep] ++ unavail
    --G.comboBoxSetModel combo (Just store)
    G.comboBoxSetRowSeparatorSource combo (Just (store, (==sep)))

userConstraintChanged :: (D.Rel c v a s) => RSetExplorer c a -> G.TreePath -> String -> IO ()
userConstraintChanged ref (idx:_) val = do
    se <- readIORef ref
    entries <- G.listStoreToList $ seStore se
    let selects = map varUserSelectionText entries
    updateStore ref $ take idx selects ++ [val] ++ drop (idx+1) selects


---------------------------------------------------------------------
-- Private functions
---------------------------------------------------------------------

entryColor :: VarEntry a -> G.Color
entryColor e | not (varEnabled e) = colorDisabled
             | varChanged e       = colorChanged
             | otherwise          = colorNormal

listStoreFromList :: G.ListStore a -> [a] -> IO()
listStoreFromList ls xs = do mapIdxM (\x id -> G.listStoreSetValue ls id x) xs
                             return ()

supportVars :: (D.Rel c v a s, ?m::c) => [VarEntry a] -> a -> S.Set String
supportVars entries rel = S.fromList 
                          $ map varName 
                          $ filter (any (\idx -> elem idx support) . varIndices)
                          $ entries
    where support = supportIndices rel

varUserConstraint :: (D.Rel c v a s, ?m::c) => VarEntry a -> a
varUserConstraint var = constraintFromStr var (varUserSelectionText var)

varAssignmentStr :: (D.Rel c v a s, ?m::c) => VarEntry a -> String
varAssignmentStr var = constraintToStr var (varAssignment var)

constraintFromStr :: (D.Rel c v a s, ?m::c) => VarEntry a -> String -> a
constraintFromStr var@VarEntry{..} str =
    case str of 
         ""  -> t
         "*" -> t
         _   -> case varType of
                   D.SInt _  -> case ichoice of 
                                     Nothing   -> t
                                     Just ival -> eqConst v ival
                   D.UInt _  -> case ichoice of 
                                     Nothing   -> t
                                     Just ival -> eqConst v ival
                   D.Bool    -> case str of
                                     "true"  -> eqConst v (1::Int)
                                     "false" -> eqConst v (0::Int)
                   D.Enum es -> eqConst v (fromJust $ findIndex (==str) es)
    where ichoice::(Maybe Integer) = readMay str
          v = varVar var
         
constraintToStr :: (D.Rel c v a s, ?m::c) => VarEntry a -> a -> String
constraintToStr _ rel            | rel == t = "*"
constraintToStr _ rel            | rel == b = "#"
constraintToStr var@VarEntry{..} rel = 
    D.valStrFromInt varType $ boolArrToBitsBe $ extract (varVar var) $ fromJust $ satOne rel
    
-- transition relation or user selection has changed--update the store
updateStore :: (D.Rel c v a s) => RSetExplorer c a -> [String] -> IO ()
updateStore ref selects = do
    se@(SetExplorer {..}) <- readIORef ref
    let ?m = seCtx
    -- update store
    entries <- G.listStoreToList seStore
    let entries'  = map (\(e,s) -> e{varUserSelectionText = s}) $ zip entries selects
        rels      = conj $ map varUserConstraint entries'
        rel'      = rels .& seRel
        support   = supportVars entries rel'
        entries'' = map (\e -> e{varEnabled = S.member (varName e) support}) entries'
    listStoreFromList seStore entries''

    -- Recompute prime implicants
    writeIORef ref $ se {seCover = primeCover rel'}
    showImplicant ref 0

showImplicant :: (D.Rel c v a s, ?m::c) => RSetExplorer c a -> Int -> IO ()
showImplicant ref idx = do
    SetExplorer {..} <- readIORef ref
    entries <- G.listStoreToList seStore
    let remaining = drop idx seCover
        entries' = map (\e@VarEntry{..} -> let asn = case remaining of
                                                          []  -> b
                                                          i:_ -> fromJust $ oneCube (varVar e) i
                                           in e {varAssignment = asn, varChanged = (asn /= varAssignment)})
                       entries
    listStoreFromList seStore entries'
    -- update spin button
    G.spinButtonSetValue seSpin (fromIntegral idx)
    case remaining of
         (_:_:_) -> G.spinButtonSetRange seSpin 0 (fromIntegral $ idx+1)
         _       -> G.spinButtonSetRange seSpin 0 (fromIntegral idx)
    evtValueChanged seCB
