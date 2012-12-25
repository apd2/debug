{-# LANGUAGE RecordWildCards, ImplicitParams, ScopedTypeVariables #-}

module SetExplorer(setExplorerNew,
                   setExplorerSetRelation,
                   setExplorerReset,
                   setExplorerGetVarAssignment) where

import Safe
import Data.IORef
import Data.Bits
import Data.Maybe
import Data.List
import Control.Monad
import qualified Data.Set        as S
import qualified Graphics.UI.Gtk as G

import qualified DbgTypes        as D
import Implicit

----------------------------------------------------------
-- Constants
----------------------------------------------------------

colorDisabled = G.Color 20000 20000 20000
colorChanged  = G.Color 65535 0     0
colorNormal   = G.Color 0     0     0

data SetExplorerEvents = SetExplorerEvents {
}

----------------------------------------------------------
-- Types
----------------------------------------------------------

data SetExplorer c v a = SetExplorer {
    seCtx            :: c,
    seRel            :: a,
    seVBox           :: G.VBox,
    seSpin           :: G.SpinButton,
    seStore          :: G.ListStore (VarEntry v a),
    seCover          :: [a]          -- prime cover of seRel conjuncted with user selections
}

type RSetExplorer c v a = IORef (SetExplorer c v a)

data VarEntry v a = VarEntry {
    varName              :: String,
    varVar               :: v,
    varType              :: D.Type,
    varIndices           :: [Int],
    varUserSelectionText :: String,
    varAssignment        :: a,        -- selected variable value(s)
    varEnabled           :: Bool,     -- False = don't care variable
    varChanged           :: Bool      -- True = highlight variable as changed
}

----------------------------------------------------------
-- External interface
----------------------------------------------------------

setExplorerNew :: c -> [(String, v, D.Type, [Int])] -> SetExplorerEvents -> IO (RSetExplorer c v a)
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
    G.boxPackStart vbox spin G.PackNatural 0
    G.widgetShow spin

    -- list store
    let entries = map (\(n,v,d,i) -> VarEntry { varName              = n
                                              , varVar               = v
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

    let addColumn title renderers = do
            col <- G.treeViewColumnNew
            G.treeViewColumnSetTitle col title
            mapM (\(rend,func) -> do G.cellLayoutPackStart col rend False
                                     G.cellLayoutSetAttributeFunc col rend store func)
                 renderers
            G.treeViewAppendColumn view col

    let nodeFromIter iter = do
            let idx = G.listStoreIterToIndex iter
            G.listStoreGetValue store idx

    -- Variable column
    varNameRend <- G.cellRendererTextNew
    let nameAttrFunc iter = do
            e@VarEntry{..} <- nodeFromIter iter 
            G.set varNameRend [G.cellText G.:= varName,
                               G.cellTextForegroundColor G.:= entryColor e]
    addColumn "Variable" [(varNameRend,nameAttrFunc)]

    -- Type column
    typeRend <- G.cellRendererTextNew
    let typeAttrFunc iter = do
            e@VarEntry{..} <- nodeFromIter iter
            G.set typeRend [G.cellText G.:= show varType,
                            G.cellTextForegroundColor G.:= entryColor e]        
    addColumn "Type" [(typeRend,typeAttrFunc)]

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

    addColumn "Constraint" [(constrTextRend, textSetFunc), (constrComboRend, comboSetFunc)]

    -- Variable assignment column
    valRend <- G.cellRendererTextNew
    let valAttrFunc iter = do
            e <- nodeFromIter iter
            G.set valRend [G.cellText G.:= varAssignmentStr e,
                           G.cellTextForegroundColor G.:= entryColor e]        
    addColumn "Value" [(valRend,valAttrFunc)]

    ref <- newIORef $ SetExplorer { seCtx   = ctx
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
    
setExplorerSetRelation :: RSetExplorer c v a -> a -> IO ()
setExplorerSetRelation ref rel = do
    se <- readIORef ref
    let ?m = seCtx se
    let se' = se {seRel = rel}
    writeIORef ref se'
    entries <- G.listStoreToList $ seStore se'
    updateStore ref $ map varUserSelectionText entries

-- Clear all value selections
setExplorerReset :: RSetExplorer c v a -> IO ()
setExplorerReset ref = updateStore ref $ repeat "*"

setExplorerGetVarAssignment :: RSetExplorer c v a -> IO [(String, a)]
setExplorerGetVarAssignment ref = do
    se <- readIORef ref
    let ?m = seCtx se
    entries <- G.listStoreToList $ seStore se
    return $ map (\e -> (varName e, varAssignment e)) entries

---------------------------------------------------------------------
-- GUI event handlers
---------------------------------------------------------------------

userConstraintSelectionStarted :: RSetExplorer c v a -> G.Widget -> G.TreePath -> IO ()
userConstraintSelectionStarted ref w (idx:_) = do
    se@SetExplorer{..} <- readIORef ref
    entries <- G.listStoreToList seStore idx
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

userConstraintChanged :: RSetExplorer c v a -> G.TreePath -> String -> IO ()
userConstraintChanged ref (idx:_) val = do
    se <- readIORef ref
    entries <- G.listStoreToList $ seStore se
    let selects = map varUserSelectionText entries
    updateStore ref $ take idx selects ++ [val] ++ drop (idx+1) selects


---------------------------------------------------------------------
-- Private functions
---------------------------------------------------------------------

entryColor :: VarEntry v a -> G.Color
entryColor e | not (varEnabled e) = colorDisabled
             | varChanged e       = colorChanged
             | otherwise          = colorNormal

listStoreFromList :: G.ListStore a -> [a] -> IO()
listStoreFromList ls xs = mapM (\(x,id) -> G.listStoreSetValue ls id x) $ zip xs [0..]

supportVars :: [VarEntry v a] -> a -> S.Set String
supportVars entries rel = S.fromList 
                          $ map varName 
                          $ filter (any (\idx -> S.member idx support) . varIndices)
                          $ entries
    where support = S.fromList $ supportIndices rel

varUserConstraint :: VarEntry v a -> a
varUserConstraint var = constraintFromStr var (varUserSelectionText var)

varAssignmentStr :: VarEntry v a -> String
varAssignmentStr var = constraintToStr var (varAssignment var)

constraintFromStr :: VarEntry v a -> String -> a
constraintFromStr VarEntry{..} str =
    case str of 
         ""  -> t
         "*" -> t
         _   -> case varType of
                   D.SInt _  -> case ichoice of 
                                     Nothing   -> t
                                     Just ival -> eqConst varVar ival
                   D.UInt _  -> case ichoice of 
                                     Nothing   -> t
                                     Just ival -> eqConst varVar ival
                   D.Bool    -> case str of
                                     "true"  -> eqConst varVar (1::Int)
                                     "false" -> eqConst varVar (0::Int)
                   D.Enum es -> eqConst varVar (fromJust $ findIndex (==str) es)
    where ichoice::(Maybe Integer) = readMay str
         
constraintToStr :: VarEntry v a -> a -> String
constraintToStr _ rel            | rel == t = "*"
constraintToStr _ rel            | rel == b = "#"
constraintToStr VarEntry{..} rel = 
    valStrFromInt varType $ boolArrToBitsBe $ extract varVar $ fromJust $ satOne rel
    
boolArrToBitsBe :: (Bits a) => [Bool] -> a
boolArrToBitsBe bits = foldl' (\x (bit, id) -> if bit then setBit x id else x) 0 $ zip (reverse bits) [0..]

valStrFromInt :: D.Type -> Integer -> String
valStrFromInt D.Bool      0                    = "False"
valStrFromInt D.Bool      1                    = "True"
valStrFromInt (D.Enum es) i | length es >= i+1 = es !! i
                            | otherwise        = "?"
valStrFromInt _           i                    = show i

-- transition relation or user selection has changed--update the store
updateStore :: RSetExplorer c v a -> [String] -> IO ()
updateStore ref selects = do
    se@(SetExplorer {..}) <- readIORef ref
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

showImplicant :: RSetExplorer c v a -> Int -> IO ()
showImplicant ref idx = do
    SetExplorer {..} <- readIORef ref
    entries <- G.listStoreToList seStore
    let remaining = drop idx seCover
        entries' = map (\e@VarEntry{..} -> let asn = case remaining of
                                                          []  -> b
                                                          i:_ -> fromJust $ oneCube varVar i
                                           in e {varAssignment = asn, varChanged = (asn /= varAssignment)})
                       entries
    listStoreFromList seStore entries'
    -- update spin button
    G.spinButtonSetValue seSpin idx
    case remaining of
         (_:_:_) -> G.spinButtonSetRange seSpin 0 (idx+1)
         _       -> G.spinButtonSetRange seSpin 0 idx
