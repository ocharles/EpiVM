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

> data Infix = Plus  | Minus | Times | Divide | Append
>            | Equal | Lt    | Gt

> type Defs = [(Name, Lang)]

> build :: Lang -> Term
> build (Lam f) = term (\x -> build (f (EpicRef x)))
> build (EpicRef x) = term x
> build (Ref n) = ref n
> build (App f a) = build f @@ build a
> build (Const (CInt x)) = int x
> build (Const (CStr x)) = str x
> build (Op Append l r) = fn "append" @@ build l @@ build r
> build (Op op l r) = op_ (buildOp op) (build l) (build r)
>  where buildOp Plus   = plus_
>        buildOp Minus  = minus_
>        buildOp Times  = times_
>        buildOp Divide = divide_
>        buildOp Equal  = eq_
>        buildOp Lt     = lt_
>        buildOp Gt     = gt_

> mkProg :: Defs -> Program
> mkProg ds = mkProgram $ basic_defs ++ map (\ (n, d) -> EpicFn n (build d)) ds

> execute :: Defs -> IO ()
> execute p = run (mkProg p)
