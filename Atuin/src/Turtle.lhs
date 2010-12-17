> module Turtle where

> type Id = [String]
> type Root = String

> mkId :: String -> Id
> mkId = (:[])

> data Exp = Infix Op Exp Exp
>          | Var Id
>          | Const Const
>   deriving Show

> data Const = MkInt Int
>            | MkString String
>            | MkChar Char
>            | MkBool Bool
>            | MkCol Colour
>   deriving Show

> data Colour = Black | Red | Green | Blue | Yellow | Cyan | Magenta | White
>   deriving (Show, Eq)

> data Turtle = Call Id [Exp]
>             | Turtle Command
>             | Seq Turtle Turtle
>             | If Exp Turtle Turtle
>             | Repeat Exp Turtle
>             | Let Id Exp Turtle
>             | Pass
>   deriving Show

> type Function = ([Id], Turtle)

> data Op = Plus | Minus | Times  | Divide      -- int ops
>         | Eq   | LT    | LE     | GT     | GE -- bool ops  
>         | Car  | Cdr   | Append | Index       -- TODO: string/char ops
>   deriving Show

> data Command = Fd Exp
>              | Rt Exp
>              | Lt Exp
>              | Colour Exp
>              | PenUp
>              | PenDown
>    deriving Show

