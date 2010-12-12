> {-# OPTIONS_GHC -fglasgow-exts #-}

> module Epic.Epic(module Epic.Epic, 
>                  Expr, Op(..)) where

Combinators for constructing an expression

> import Control.Monad.State
> import System
> import System.IO

> import Epic.Language
> import Epic.Compiler

Allow Haskell functions to be used to build expressions.

> class EpicExpr e where
>     expr :: e -> State Int Expr

> instance EpicExpr Expr where
>     expr e = return e

> instance EpicExpr (State Int Expr) where
>     expr e = e

> type Term = State Int Expr

> instance (EpicExpr e) => EpicExpr (Expr -> e) where
>     expr f = do var <- get
>                 put (var+1)
>                 let arg = MN "evar" var
>                 e' <- expr (f (R arg))
>                 return (Lam arg TyAny e')

> instance EpicExpr ([Name], Expr) where
>     expr (ns, e) = lam ns e where
>         lam [] e = return e
>         lam (n:ns) e = do e' <- lam ns e
>                           return (Lam n TyAny e')

> class EpicFn e where
>     func :: e -> State Int Func

> instance EpicFn Expr where
>     func e = return (Bind [] 0 e [])

> instance EpicFn Term where
>     func e = do e' <- e
>                 return (Bind [] 0 e' [])

> instance (EpicFn e) => EpicFn (Expr -> e) where
>     func f = do var <- get
>                 put (var+1)
>                 let arg = MN "evar" var
>                 (Bind vars l e' flags) <- func (f (R arg))
>                 return (Bind ((arg, TyAny):vars) l e' flags)

> instance EpicFn ([Name], Expr) where
>     func (ns, e) = return (Bind (map (\x -> (x, TyAny)) ns) 0 e [])

Use arithmetic operators for expressions

> instance Num Expr where
>     (+) = Op Plus
>     (-) = Op Minus
>     (*) = Op Times
>     negate x = Const (MkInt 0) - x
>     abs = undefined
>     signum = undefined
>     fromInteger x = Const (MkInt (fromInteger x))

> instance Fractional Expr where
>     (/) = Op Divide
>     fromRational x = Const (MkFloat (fromRational x))

Binary operators

> eq = Op OpEQ
> lt = Op OpLT
> lte = Op OpLE
> gt = Op OpGT
> gte = Op OpGE

> mkFunc :: EpicFn e => e -> Func
> mkFunc e = evalState (func e) 0

Build case expressions. Allow functions to be used to bind names in
case alternatives

> infixl 5 <|>

> class Cases c where
>     (<|>) :: Cases d => c -> d -> [CaseAlt]
>     alt :: c -> [CaseAlt]

>     (<|>) c1 c2 = alt c1 ++ alt c2

> instance Cases CaseAlt where
>     alt c = [c]

> instance (Cases c) => Cases [c] where
>     alt cs = concatMap alt cs

> class Alternative e where
>     mkAlt :: Tag -> e -> State Int CaseAlt

> instance Alternative Expr where
>     mkAlt t e = return (Alt t [] e)

> instance Alternative Term where
>     mkAlt t e = do e' <- e
>                    return (Alt t [] e')

> instance (Alternative e) => Alternative (Expr -> e) where
>     mkAlt t f = do var <- get
>                    put (var+1)
>                    let arg = MN "alt" var
>                    (Alt t vars e') <- mkAlt t (f (R arg))
>                    return $ Alt t ((arg, TyAny):vars) e'

> instance Alternative ([Name], Expr) where
>     mkAlt t (vars, e) = return $ Alt t (map (\x -> (x, TyAny)) vars) e

> con :: Alternative e => Int -> e -> State Int CaseAlt
> con t e = mkAlt t e

> const = ConstAlt
> defaultcase = DefaultCase

Remaining expression constructs

> exp1 :: (EpicExpr a) =>
>         (Expr -> Expr) -> a -> Term
> exp1 f a = do a' <- expr a
>               return (f a')

> exp2 :: (EpicExpr a, EpicExpr b) =>
>         (Expr -> Expr -> Expr) -> a -> b -> Term
> exp2 f a b = do a' <- expr a; b'<- expr b
>                 return (f a' b')

> exp3 :: (EpicExpr a, EpicExpr b, EpicExpr c) =>
>         (Expr -> Expr -> Expr -> Expr) -> a -> b -> c -> Term
> exp3 f a b c = do a' <- expr a; b'<- expr b; c' <- expr c
>                   return (f a' b' c')

> if_ :: (EpicExpr a, EpicExpr t, EpicExpr e) =>
>        a -> t -> e -> Term
> if_ = exp3 If

> while_ :: (EpicExpr t, EpicExpr b) =>
>           t -> b -> Term
> while_ = exp2 While

> whileAcc_ :: (EpicExpr a, EpicExpr t, EpicExpr e) =>
>              a -> t -> e -> Term
> whileAcc_ = exp3 WhileAcc

> error_ :: String -> Term
> error_ str = return (Error str)

> op_ :: (EpicExpr a, EpicExpr b) => Op -> a -> b -> Term
> op_ op = exp2 (Op op)

> foreign_ = ForeignCall
> foreignL_ = LazyForeignCall

 mkCon :: Int -> [Term] -> Term
 mkCon tag args = do args' <- mapM expr args
                     return (Con tag args')

> con_ :: Int -> Term
> con_ t = return (Con t [])

> case_ :: (EpicExpr e) => e -> [State Int CaseAlt] -> Term
> case_ e alts = do e' <- expr e
>                   alts' <- mapM id alts
>                   return (Case e' alts')

> letN_ :: (EpicExpr val, EpicExpr scope) =>
>          Name -> val -> scope -> Term
> letN_ n val sc = do val' <- expr val
>                     sc' <- expr sc
>                     return $ Let n TyAny val' sc'

> let_ :: (EpicExpr e) =>
>         e -> (Expr -> Term) -> Term
> let_ e f = do e' <- expr e
>               f' <- f (R (MN "DUMMY" 0))
>               let var = MN "loc" (topVar f')
>               fv <- f (R var)
>               return $ Let var TyAny e' fv

> maxs = foldr max 0

> topVar (Let (MN "loc" x) _ _ _) = x+1
> topVar (Let _ _ e1 e2) = max (topVar e1) (topVar e2)
> topVar (App f as) = max (topVar f) (maxs (map topVar as))
> topVar (Lazy e) = topVar e
> topVar (Effect e) = topVar e
> topVar (Con t es) = maxs (map topVar es)
> topVar (Proj e i) = topVar e
> topVar (If a t e) = max (max (topVar a) (topVar t)) (topVar e)
> topVar (While a e) = max (topVar a) (topVar e)
> topVar (WhileAcc a t e) = max (max (topVar a) (topVar t)) (topVar e)
> topVar (Op op a e) = max (topVar a) (topVar e)
> topVar (WithMem a e1 e2) = max (topVar e1) (topVar e2)
> topVar (ForeignCall t s es) = maxs (map topVar (map fst es))
> topVar (LazyForeignCall t s es) = maxs (map topVar (map fst es))
> topVar (Case e alts) = max (topVar e) (maxs (map caseLet alts))
>   where caseLet (Alt t n e) = topVar e
>         caseLet (ConstAlt t e) = topVar e
>         caseLet (DefaultCase e) = topVar e
> topVar _ = 0


> str :: String -> Expr
> str x = Const (MkString x)

> int :: Int -> Expr
> int x = Const (MkInt x)

> float :: Float -> Expr
> float x = Const (MkFloat x)

> char :: Char -> Expr
> char x = Const (MkChar x)


> infixl 1 +>

> (+>) :: (EpicExpr c) => c -> Term -> Term
> (+>) c k = let_ c (\x -> k)

> tyInt, tyChar, tyBool, tyFloat, tyString, tyPtr, tyUnit, tyAny :: Type
> tyC :: String -> Type

> tyInt    = TyInt
> tyChar   = TyChar
> tyBool   = TyBool
> tyFloat  = TyFloat
> tyString = TyString
> tyPtr    = TyPtr
> tyUnit   = TyUnit
> tyAny    = TyAny
> tyC      = TyCType

> infixl 5 !., @@

> (!.) = Proj

> fn = R . UN

> (@@) :: (EpicExpr f, EpicExpr a) => f -> a -> Term
> (@@) f a = do f' <- expr f
>               a' <- expr a
>               case f' of
>                 App fi as -> return $ App fi (as ++ [a'])
>                 Con t as -> return $ Con t (as ++ [a'])
>                 _ -> return $ App f' [a']

> data EpicDecl = forall e. EpicFn e => EpicFn e
>               | Extern Name Type [Type]
>               | Include String
>               | Link String
>               | CType String

> instance Show EpicDecl where
>     show (EpicFn e) = show (evalState (func e) 0)


> type Program = [(Name, EpicDecl)]

> name :: String -> Name
> name = UN

> mkDecl :: (Name, EpicDecl) -> Decl
> mkDecl (n, EpicFn e) = Decl n TyAny (mkFunc e) Nothing []
> mkDecl (n, Epic.Epic.Extern nm ty tys) = Epic.Language.Extern nm ty tys
> mkDecl (n, Epic.Epic.Include f) = Epic.Language.Include f
> mkDecl (n, Epic.Epic.Link f) = Epic.Language.Link f
> mkDecl (n, Epic.Epic.CType f) = Epic.Language.CType f

> compile :: Program -> FilePath -> IO ()
> compile tms outf = do compileDecls (outf++".o") Nothing (map mkDecl tms) []
>                       Epic.Compiler.link [outf++".o"] [] outf True []

> -- |Compile a program to a .o
> compileObj :: Program -> FilePath -> IO ()
> compileObj tms outf = compileDecls outf Nothing (map mkDecl tms) []

> -- |Link a collection of object files. By convention, the entry point is
> -- the function called "main".
> link :: [FilePath] -> FilePath -> IO ()
> link fs outf = Epic.Compiler.link fs [] outf True []

> run :: Program -> IO ()
> run tms = do (tmpn, tmph) <- tempfile
>              hClose tmph
>              Epic.Epic.compile tms tmpn
>              system tmpn
>              return ()

Some useful functions

> putStr_ x = foreign_ tyUnit "putStr" [(x, tyString)]

> putStrLn_ :: Expr -> Term
> putStrLn_ x = (fn "putStr") @@ ((fn "append") @@ x @@ str "\n")

> readStr_ = foreign_ tyString "readStr" []

> append_ x y = foreign_ tyString "append" [(x, tyString), (y, tyString)]
> intToString_ x = foreign_ tyString "intToStr" [(x, tyInt)]

> -- |Some default definitions: putStr, putStrLn, readStr, append
> basic_defs = [(name "putStr",      EpicFn putStr_),
>               (name "putStrLn",    EpicFn putStrLn_),
>               (name "readStr",     EpicFn readStr_),
>               (name "append",      EpicFn append_),
>               (name "intToString", EpicFn intToString_)]

