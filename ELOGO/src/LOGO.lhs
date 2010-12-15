> module LOGO where

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
>            | MkCol Colour
>   deriving Show

> data Colour = Black | Red | Green | Blue | Yellow | Cyan | Magenta | White
>   deriving Show

> data LOGO = Call Id [Exp]
>           | Turtle Command
>           | Seq LOGO LOGO
>           | Let Id Exp LOGO
>   deriving Show

> type Function = ([Id], LOGO)

> data Op = Plus | Minus | Times  | Divide      -- int ops
>         | Eq   | LT    | LE     | GT     | GE -- bool ops  
>         | Car  | Cdr   | Append | Index       -- string/char ops
>   deriving Show

> data Command = Fd Exp
>              | Rt Exp
>              | Lt Exp
>              | Color Exp
>              | PenUp
>              | PenDown
>    deriving Show

