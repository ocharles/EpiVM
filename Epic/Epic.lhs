> {-# OPTIONS_GHC -fglasgow-exts #-}

> module Epic.Epic(module Epic.Epic, 
>                  Expr, Op(..)) where

Combinators for constructing an expression

> import Control.Monad.State
> import System
> import System.IO

> import Epic.Language
> import Epic.Compiler

Allow Haskell functions to be used to build expressions, as long as they
are top level functions.

> class Epic e where
>     expr :: e -> State Int Func

> instance Epic Expr where
>     expr e = return (Bind [] 0 e [])

> instance (Epic e) => Epic (Expr -> e) where
>     expr f = do var <- get
>                 put (var+1)
>                 let arg = MN "evar" var
>                 (Bind vars l e' flags) <- expr (f (R arg))
>                 return (Bind ((arg, TyAny):vars) l e' flags)

> instance Epic ([Name], Expr) where
>     expr (ns, e) = return (Bind (map (\x -> (x, TyAny)) ns) 0 e [])

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

> mkFunc :: Epic e => e -> Func
> mkFunc e = evalState (expr e) 0

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

> instance (Alternative e) => Alternative (Expr -> e) where
>     mkAlt t f = do var <- get
>                    put (var+1)
>                    let arg = MN "alt" var
>                    (Alt t vars e') <- mkAlt t (f (R arg))
>                    return $ Alt t ((arg, TyAny):vars) e'

> instance Alternative ([Name], Expr) where
>     mkAlt t (vars, e) = return $ Alt t (map (\x -> (x, TyAny)) vars) e

> con :: Alternative e => Int -> e -> CaseAlt
> con t e = evalState (mkAlt t e) 0

> const = ConstAlt
> defaultcase = DefaultCase

Remaining expression constructs

> if_ = If
> while_ = While
> whileAcc_ = WhileAcc
> case_ = Case
> apply_ = App
> error_ = Error
> var x = R (UN x) -- global names, local names are lambda bound
> mkCon = Con
> op_ = Op
> foreign_ = ForeignCall
> foreignL_ = LazyForeignCall

> let_ e f = let var = MN "loc" (topLet (f (R (MN "DUMMY" 0)))) in
>            Let var TyAny e (f (R var))

> maxs = foldr max 0

> topLet (Let (MN "loc" x) _ _ _) = x+1
> topLet (Let _ _ e1 e2) = max (topLet e1) (topLet e2)
> topLet (App f as) = max (topLet f) (maxs (map topLet as))
> topLet (Lazy e) = topLet e
> topLet (Effect e) = topLet e
> topLet (Con t es) = maxs (map topLet es)
> topLet (Proj e i) = topLet e
> topLet (If a t e) = max (max (topLet a) (topLet t)) (topLet e)
> topLet (While a e) = max (topLet a) (topLet e)
> topLet (WhileAcc a t e) = max (max (topLet a) (topLet t)) (topLet e)
> topLet (Op op a e) = max (topLet a) (topLet e)
> topLet (WithMem a e1 e2) = max (topLet e1) (topLet e2)
> topLet (ForeignCall t s es) = maxs (map topLet (map fst es))
> topLet (LazyForeignCall t s es) = maxs (map topLet (map fst es))
> topLet (Case e alts) = max (topLet e) (maxs (map caseLet alts))
>   where caseLet (Alt t n e) = topLet e
>         caseLet (ConstAlt t e) = topLet e
>         caseLet (DefaultCase e) = topLet e
> topLet _ = 0

> str x = Const (MkString x)

> infixl 1 +>
> (+>) c k = Let (MN "discard" 0) TyAny c k

> tyInt    = TyInt
> tyChar   = TyChar
> tyBool   = TyBool
> tyFloat  = TyFloat
> tyString = TyString
> tyPtr    = TyPtr
> tyUnit   = TyUnit
> tyAny    = TyAny
> tyC      = TyCType

> infixl 5 !., <$>

> (!.) = Proj

> (<$>) nm args = apply_ (R (UN nm)) args

> data EpicTm = forall e. Epic e => Epic e
>             | Extern Name Type [Type]
>             | Include String
>             | Link String
>             | CType String

> type Program = [(Name, EpicTm)]

> name :: String -> Name
> name = UN

> mkDecl :: (Name, EpicTm) -> Decl
> mkDecl (n, Epic e) = Decl n TyAny (mkFunc e) Nothing []
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

> -- |Link a collection of object files
> link :: [FilePath] -> FilePath -> IO ()
> link fs outf = Epic.Compiler.link fs [] outf True []

> run :: Program -> IO ()
> run tms = do (tmpn, tmph) <- tempfile
>              hClose tmph
>              Epic.Epic.compile tms tmpn
>              system tmpn
>              return ()

