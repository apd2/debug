module DbgAbstract () where

abstractTransition :: RSourceView c a -> State a Store -> Store -> IO (Transition a Store)
abstractTransition ref from to =
    SourceView{..} <- readIORef ref
    model <- readIORef $ svModel sv
    let tranFrom = from
        -- compute untracked and label predicates over to
        tranUntracked = conj $ map (evalAbsVar sv to) D.mUntrackedVars
        tranAbstractLabel = conj $ map (evalAbsVar sv to) D.mLabelVars
        -- project "to" state on "tmp" variables
        tranConcreteLabel = Just $ storeProject to (map I.varName $ C.specTmpVars svSpec)
        tranTo = D.State { sAbstract = conj $ map (evalAbsVar sv to) D.mCurStateVars
                         , sConcrete = Just $ storeProject to (map I.varName $ C.specVar svSpec)}
    return $ Transition{..}


evalAbsVar :: SourceView c a -> Store -> ModelVar -> a
evalAbsVar sv store ModelVar{..} = evalAbsVar' store (svAbsVars M.! mvarName) mvarIdx

evalAbsVar' :: Store -> AbsVar -> [Int] -> a
evalAbsVar' store (AVarPred p) is = 
    if storeEvalBool store (predToExpr p)
       then eqConst (D.idxToVS is) 1
       else eqConst (D.idxToVS is) 0

evalAbsVar' store (AVarTerm term) is = 
   eqConst (D.idxToVS is) $ scalarToInt $ storeEvalScalar store (I.termToExpr term)

--evalAbsVar' store (AVarEnum name vals) is =
--    case findIndex (==v) vals of
--         Just i  -> eqConst vs i
--         Nothing -> conj $ map (nt $ eqConst vs i) [0..length vals -1]
--    where v  = storeEvalScalar store (I.EVar name)
--          vs = D.idxToVS is

scalarToInt :: I.Val -> Int
scalarToInt (I.BoolVal True)  = 1
scalarToInt (I.BoolVal False) = 0
scalarToInt (I.UIntVal i)     = i
scalarToInt (I.SIntVal i)     = i
scalarToInt (I.EnumVal s)     = enumToInt s

