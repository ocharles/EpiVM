> {-# OPTIONS_GHC -fglasgow-exts #-}

> module Main where

> import Lang
> import Epic.Epic

> add :: Lang
> add = Lam (\x -> Lam (\y -> Op IPlus x y))

> main_ = App (Ref (name "putStrLn"))
>             (App (Ref (name "intToString"))
>                  (App (App (Ref (name "add")) 
>                        (Const (CInt 5))) (Const (CInt 6))))

> testdefs = [(name "add", LangDef add), (name "main", LangDef main_)]

> main = execute testdefs