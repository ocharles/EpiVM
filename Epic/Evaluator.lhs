> module Epic.Evaluator(eval) where

> import Epic.Language

Assume all expressions are in HOAS form - if we see any Vs, or any Updates
then we have an error.

> eval :: [EvalDecl] -> Expr -> Expr
> eval ctx e = case ev e of
>                Nothing -> e
>                Just e' -> e'
>   where
>     ev (R n) = case lookupD n ctx of
>                  Just e' -> ev e'
>                  Nothing -> return $ R n
>     ev (V i) = fail "Not in HOAS form"
>     ev (App f xs) = do f' <- ev f
>                        xs' <- mapM ev xs
>                        evFn f' xs'
>     ev (Lazy e) = ev e
>     ev (Effect e) = ev e
>     ev (Con t es) = do es' <- mapM ev es
>                        return $ Con t es'
>     ev (Proj e i) = do e' <- ev e
>                        return $ project e' i
>     ev (Case e alts) = do e' <- ev e
>                           docase e' alts
>     ev (If x t e) = do x' <- ev x
>                        case x of
>                          Const (MkInt 0) -> ev e
>                          _ -> ev t
>     ev (While _ _) = fail "Can't evaluate while"
>     ev (WhileAcc _ _ _) = fail "Can't evaluate while"
>     ev (Op op x y) = do x' <- ev x
>                         y' <- ev y
>                         case (x', y') of
>                           (Const xv, Const yv) -> return $ doOp op xv yv
>                           _ -> return $ Op op x' y'
>     ev (Let _ _ _ _) = fail "Not in HOAS form (let)"
>     ev (LetM _ _ _) = fail "Can't do updates"
>     ev (HLet n ty val sc) = do val' <- ev val
>                                ev (sc val')
>     ev (WithMem a t e) = ev e
>     ev (ForeignCall t str args) 
>         = do args' <- mapM ev (map fst args)
>              return $ ForeignCall t str (zip args' (map snd args))
>     ev (LazyForeignCall t str args) 
>         = do args' <- mapM ev (map fst args)
>              return $ LazyForeignCall t str (zip args' (map snd args))
>     ev x = return x

>     evFn (HLam n t sc) (a:as) = evFn (sc a) as
>     evFn f [] = ev f
>     evFn f as = return $ App f as

>     docase c@(Con t as) alts = case fConAlt t as alts of
>                                  Just rhs -> ev rhs
>                                  Nothing -> return $ Case c alts
>     docase c@(Const (MkInt i)) alts 
>                              = case fConstAlt i alts of
>                                  Just rhs -> ev rhs
>                                  Nothing -> return $ Case c alts
>     docase c alts = return $ Case c alts

> fConAlt :: Int -> [Expr] -> [CaseAlt] -> Maybe Expr
> fConAlt t args (HAlt t' n rhs : _)
>         | t == t' && n == length args =
>             substRHS args rhs
>    where
>      substRHS [] (HExp rhs) = return rhs
>      substRHS (x:xs) (HBind n ty rhsf) = substRHS xs (rhsf x)
> fConAlt t args (DefaultCase e : _) = return e
> fConAlt t args (_:xs) = fConAlt t args xs
> fConAlt t args _ = Nothing

> fConstAlt :: Int -> [CaseAlt] -> Maybe Expr
> fConstAlt t (ConstAlt t' rhs:_)
>           | t == t' = return rhs
> fConstAlt t (DefaultCase e : _) = return e
> fConstAlt t (_:xs) = fConstAlt t xs
> fConstAlt t _ = Nothing

> doOp Plus  (MkInt x) (MkInt y) = Const $ MkInt (x+y)
> doOp Minus (MkInt x) (MkInt y) = Const $ MkInt (x-y)
> doOp Times (MkInt x) (MkInt y) = Const $ MkInt (x*y)
> doOp Divide (MkInt x) (MkInt y) = Const $ MkInt (x `div` y)
> doOp Modulo (MkInt x) (MkInt y) = Const $ MkInt (x `mod` y)
> doOp OpEQ (MkInt x) (MkInt y) = bint (x==y)
> doOp OpLT (MkInt x) (MkInt y) = bint (x<y)
> doOp OpLE (MkInt x) (MkInt y) = bint (x<=y)
> doOp OpGT (MkInt x) (MkInt y) = bint (x>y)
> doOp OpGE (MkInt x) (MkInt y) = bint (x>=y)

> doOp FPlus  (MkFloat x) (MkFloat y) = Const $ MkFloat (x+y)
> doOp FMinus (MkFloat x) (MkFloat y) = Const $ MkFloat (x-y)
> doOp FTimes (MkFloat x) (MkFloat y) = Const $ MkFloat (x*y)
> doOp FDivide (MkFloat x) (MkFloat y) = Const $ MkFloat (x/y)
> doOp OpFEQ (MkFloat x) (MkFloat y) = bint (x==y)
> doOp OpFLT (MkFloat x) (MkFloat y) = bint (x<y)
> doOp OpFLE (MkFloat x) (MkFloat y) = bint (x<=y)
> doOp OpFGT (MkFloat x) (MkFloat y) = bint (x>y)
> doOp OpFGE (MkFloat x) (MkFloat y) = bint (x>=y)

> doOp op x y = Op op (Const x) (Const y)

> bint True = Const $ MkInt 1
> bint False = Const $ MkInt 0

> project :: Expr -> Int -> Expr
> project (Con t as) i | i < length as = as!!i
> project e i = Proj e i

> lookupD n [] = Nothing
> lookupD n (EDecl en def:xs) | n == en = Just def
> lookupD n (_:xs) = lookupD n xs

