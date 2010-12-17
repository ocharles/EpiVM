> module Main where

> import Parser
> import MkEpic

> import System

> usage [inf, outf] = return (inf, outf)
> usage _ = fail "Usage: atuin [input] [output]"

> main :: IO ()
> main = do args <- getArgs
>           (inf, outf) <- usage args
>           putStrLn $ "Compiling " ++ inf ++ " to " ++ outf
>           prog <- parseFile (args!!0)
>           case prog of
>                Left (e, f, l) -> putStrLn $ f ++ ":" ++ show l ++ ":" ++ e
>                Right p -> output p (args !! 1)
