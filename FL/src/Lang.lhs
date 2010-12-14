> {-# OPTIONS_GHC -fglasgow-exts #-}

> module Lang where

> import Epic.Epic

> data Lang = Lam (Lang -> Lang)
>           | Ref Name
>           | App Lang Lang
>           | Const Const
>           | Op Infix Lang Lang
>           | EpicRef Expr -- for conversion of Lam to Epic expressions

> data Const = CInt Int
>            | CStr String

> data Infix = IPlus | IMinus | ITimes | IDivide | IAppend
>            | IEQ | ILT | IGT

> data Def = LangDef Lang
>          | PrimDef EpicDecl

> type Defs = [(Name, Def)]

> build :: Lang -> Term
> build (Lam f) = term (\x -> build (f (EpicRef x)))
> build (EpicRef x) = term x
> build (Ref n) = term (ref n)
> build (App f a) = build f @@ build a
> build (Const (CInt x)) = term $ int x
> build (Const (CStr x)) = term $ str x
> build (Op IAppend l r) = fn "append" @@ build l @@ build r
> build (Op op l r) = op_ (buildOp op) (build l) (build r)
>  where buildOp IPlus = Plus
>        buildOp IMinus = Minus
>        buildOp ITimes = Times
>        buildOp IDivide = Divide
>        buildOp IEQ = OpEQ
>        buildOp ILT = OpLT
>        buildOp IGT = OpGT

> mkEpic :: Def -> EpicDecl
> mkEpic (PrimDef p) = p
> mkEpic (LangDef l) = EpicFn (build l)

> mkProgram :: Defs -> Program
> mkProgram ds = basic_defs ++ map (\ (n, d) -> (n, mkEpic d)) ds

> execute :: Defs -> IO ()
> execute p = run (mkProgram p)
