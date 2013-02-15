module Store (Store(..),
              storeUnion,
              storeUnions,
              storeProject,
              storeNonDet,
              storeEval,
              storeEvalEnum,
              storeEvalBool,
              storeSet) where

import IExpr


-- Variable assignments
Store = SStruct [(String, Store)] -- name/value pairs (used for structs and for top-level store)
      | SArr    [Store]           -- array assignment
      | SVal    (Maybe I.Val)     -- scalar

-- shallow union of stores
storeUnion :: Store -> Store -> Store
storeUnion (SStruct entries1) (SStruct entries2) = SStruct $ entries1 ++ entries2

storeUnions :: [Store] -> Store
storeUnions stores = foldl' (\s1 s2 -> storeUnion s1 s2) (SStruct []) stores

storeNonDet :: I.Type -> Store
storeNonDet (I.Struct fs) = SStruct $ map (\I.Field n t -> (n, storeNonDet t)) fs
storeNonDet (Array t sz)  = SArr $ replicate sz $ storeNonDet t

-- project store on a subset of variables
storeProject :: Store -> [String] -> Store
storeProject (SStruct entries) names = SStruct $ filter (\(n,_) -> elem n names) entries

-- Expression evaluation over stores
storeTryEval :: Store -> I.Expr -> Maybe Store
storeTryEval (SStruct fs) (EVar name)       = fmap snd $ find (==name . fst) fs
storeTryEval _            (EVar _)          = Nothing
storeTryEval _            (EConst v)        = SVal $ Just v
storeTryEval s            (EField e name)   = join $ fmap (\fs -> fmap snd $ find (==name . fst) fs) $ storeTryEvalStruct s e 
storeTryEval _            (EField _ _)      = Nothing
storeTryEval s            (EIndex e i)      = do idx <- storeTryEvalInt s i
                                                 es  <- storeTryEvalArr s e
                                                 if (idx >= 0 && idx < length es)
                                                    then return $ es !! idx
                                                    else Nothing
storeTryEval s            (EUnOp op e)      = fmap (\v -> evalConstExpr (EUnOp op $ EConst v)) $ storeTryEvalScalar s e
storeTryEval s            (EBinOp op e1 e2) = do v1 <- storeTryEvalScalar s e1
                                                 v2 <- storeTryEvalScalar s e2
                                                 return $ evalConstExpr (EBinOp op (EConst v1) (EConst v2))
storeTryEval s            (ESlice e s)      = fmap (\v -> evalConstExpr (ESlice (EConst e) s)) $ storeTryEvalScalar e

storeTryEvalScalar :: Store -> I.Expr -> Maybe Val
storeTryEvalScalar s e = case storeTryEval s e of
                              Just (SVal (Just v)) -> return v
                              _                    -> Nothing

storeTryEvalInt :: Store -> I.Expr -> Maybe Integer
storeTryEvalInt s e = case storeTryEvalScalar s e of
                           Just (SIntVal _ i) -> Just i
                           Just (UIntVal _ i) -> Just i
                           _                  -> Nothing

storeTryEvalBool :: Store -> I.Expr -> Maybe Bool
storeTryEvalBool s e = case storeTryEvalScalar s e of
                           Just (BoolVal b) -> Just b
                           _                -> Nothing


storeTryEvalEnum :: Store -> I.Expr -> Maybe String
storeTryEvalEnum s e = case storeTryEvalScalar s e of
                           Just (EnumVal s) -> Just s
                           _                -> Nothing

storeTryEvalStruct :: Store -> I.Expr -> Maybe [(String, Store)]
storeTryEvalStruct s e = case storeTryEval s e of
                              Just (SStruct fs) -> Just fs
                              _                 -> Nothing

storeTryEvalArr :: Store -> I.Expr -> Maybe [Store]
storeTryEvalArr s e = case storeTryEval s e of
                           Just (SArr es) -> Just es
                           _              -> Nothing

storeEval :: Store -> I.Expr -> Store
storeEval store e = case storeTryEval store e of
                         Nothing -> error "storeEval: invalid expression"
                         Just s  -> s

storeEvalScalar :: Store -> I.Expr -> I.Val
storeEvalScalar s e = case storeTryEvalScalar s e of
                           Just v -> return v
                           _      -> error "storeEvalScalar: invalid expression"

storeEvalBool :: Store -> I.Expr -> Bool
storeEvalBool store e = case storeTryEvalBool store e of
                             Nothing -> error "storeEvalBool: invalid expression"
                             Just b  -> b

storeEvalInt :: Store -> I.Expr -> Integer
storeEvalInt store e = case storeTryEvalInt store e of
                            Nothing -> error "storeEvalInt: invalid expression"
                            Just i  -> i

storeEvalEnum :: Store -> I.Expr -> String
storeEvalEnum store e = case storeTryEvalEnum of
                             Nothing -> error "storeEvalEnum: invalid expression"
                             Just s  -> s

storeSet :: Store -> Expr -> Store -> Store
storeSet s (ESlice e (l,h)) val = 
    let ?store = s 
    in let SVal (Just v') = val
       in storeUpdate s e f
       where f (SVal (Just v)) = 
                 SVal $ Just $ evalConstExpr $ econcat $
                 (if l > 0 then [exprSlice (EConst v) (0, l-1)] else []) ++
                 [v'] ++
                 (if h < typeWidth v - 1 then [exprSlice (EConst v) (h+1, typeWidth v - 1)] else [])
storeSet s e val = let ?store = s in storeUpdate s e (\_ -> val)

storeUpdate :: (?store::Store) :: Store -> Expr -> (Store -> Store) -> Store
storeUpdate (SStruct fs) (EVar vname) f = SStruct $ map (\(n,v) -> if n == vname then f v else v) fs
storeUpdate s            (EField e n) f = storeUpdate s e (\s' -> storeUpdate s' (EVar n) f)
storeUpdate s            (EIndex a i) f = storeUpdate s a (\s' -> case s' of
                                                                       SArr es -> if idx >= 0 && idx < length es 
                                                                                     then SArr $ (take idx es) ++ [f $ es !! idx] ++ (drop (idx+1) es)
                                                                                     else -- we should probably do the same thing as with pointers: add error 
                                                                                          -- transitions on array dereferences, so that this cannot happen
                                                                                          error "storeUpdate: index out of bounds"
                                                                       _       -> error "storeUpdate: not an array")
                                          where idx = storeEvalInt ?store i
storeUpdate s         (EUnOp Deref e) f = storeUpdate s (I.lvalToExpr l) f
                                          where PtrVal l = storeEval ?store e
storeUpdate _            e            f = error $ "storeUpdate: " ++ show e
