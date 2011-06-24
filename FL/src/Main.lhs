> {-# OPTIONS_GHC -fglasgow-exts #-}

> module Main where

> import Lang
> import Epic.Epic

> add :: Lang
> add = Lam (\x -> Lam (\y -> Op Plus x y))

> main_ = App (Ref (name "putStrLn"))
>             (App (Ref (name "intToString"))
>                  (App (App (Ref (name "add")) 
>                        (Const (CInt 5))) (Const (CInt 6))))

> testdefs = [(name "add", add), (name "main", main_)]

> main = do let prog = mkProg testdefs
>           let addNums = build (App (App (Ref (name "add")) (Const (CInt 5))) (Const (CInt 6)))
>           let exp = evaluate prog addNums
>           print exp
>           run prog