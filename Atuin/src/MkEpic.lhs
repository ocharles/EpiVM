> module MkEpic(output, testProg) where

> import Turtle
> import SDLprims
> import Paths_atuin

> import Epic.Epic as Epic hiding (compile)

> opts = [GCCOpt "-lSDLmain -lsdl -lsdl_gfx -Wl,-framework,Cocoa",
>         MainInc "SDL/SDL.h"]

These get passed around every function and updated as we go

> fullId :: Id -> String
> fullId n = e n
>    where e [] = ""
>          e (x:xs) = "_" ++ x ++ e xs

> epicId :: Id -> Name
> epicId i = name (fullId i)

The main compiler function, turns a logo program into an Epic term

> class Compile a where
>     compile :: Expr -> a -> Term

> instance Compile Turtle where
>     compile state (Seq x y) 
>        = let_ (compile state x) (\state' -> compile state' y)
>     compile state (Turtle c)  = compile state c
>     compile state (Call i es) 
>          = app (fn (fullId i) @@ state) es
>        where app f [] = f
>              app f (e:es) = app (f @@ compile state e) es
>     compile state (Let i e scope) 
>         = letN_ (epicId i) (compile state e) (compile state scope)

> instance Compile Exp where
>     compile state (Infix op l r) 
>         = (mkOp op) (compile state l) (compile state r)
>         where mkOp Turtle.Plus = primPlus
>               mkOp Turtle.Minus = primMinus
>               mkOp Turtle.Times = primTimes
>               mkOp Turtle.Divide = primDivide
>     compile state (Var i) = ref (epicId i)
>     compile state (Const i) = compile state i

> instance Compile Const where
>     compile state (MkInt i) = mkint (int i)
>     compile state (MkString s) = mkstr (str s)
>     compile state (MkChar c) = mkchar (char c)
>     compile state (MkCol Black) = mkcol col_black
>     compile state (MkCol Red) = mkcol col_red
>     compile state (MkCol Green) = mkcol col_green
>     compile state (MkCol Blue) = mkcol col_blue
>     compile state (MkCol Yellow) = mkcol col_yellow
>     compile state (MkCol Cyan) = mkcol col_cyan
>     compile state (MkCol Magenta) = mkcol col_magenta
>     compile state (MkCol White) = mkcol col_white

> instance Compile Command where
>     compile state (Fd i) = fn "forward" @@ state @@ compile state i
>     compile state (Rt i) = fn "right"   @@ state @@ compile state i
>     compile state (Lt i) = fn "left"    @@ state @@ compile state i

Convert a function with arguments into an Epic definition. We have the
arguments in the definition, plus an additional state added by the system
which carries the turtle state and SDL surface.

> mkEpic :: (Id, Function) -> (Name, EpicDecl)
> mkEpic (i, (args, p)) 
>       = (epicId i, EpicFn (\ state -> (map epicId args, compile state p)))

> _main = ([], Seq (Turtle (Fd (Const (MkInt 100))))
>             (Seq (Turtle (Rt (Const (MkInt 90))))
>                  (Call (mkId "lines") [Const (MkInt 30)])))

> _lines 
>  = let turn = mkId "turn" in
>        ([turn], Seq (Turtle (Fd (Const (MkInt 100))))
>                (Seq (Turtle (Rt (Var turn)))
>                (Seq (Turtle (Fd (Const (MkInt 100))))
>                (Seq (Turtle (Rt (Infix Turtle.Times (Const (MkInt 2)) (Var turn))))
>                     (Turtle (Fd (Const (MkInt 100))))))))

> testProg :: [(Id, Function)]
> testProg = [(mkId "main",  _main), 
>             (mkId "lines", _lines)]

Epic main program - initialises SDL, sets up an initial turtle state,
runs the program called "main" and waits for a key press.

> runMain :: Term
> runMain = 
>   let_ (fn "initSDL" @@ int 640 @@ int 480)
>   (\surface -> 
>       (fn (fullId (mkId "main")) @@ (init_turtle surface)) +>
>        flipBuffers surface +>
>        pressAnyKey)

Find the support files (the SDL glue code) and compile an Epic program
with the primitives (from SDLprims) and the user's program.

> output :: [(Id, Function)] -> FilePath -> IO ()
> output prog fp = do -- TODO: run sdl-config
>                     sdlo <- getDataFileName "sdl/sdlrun.o"
>                     sdlh <- getDataFileName "sdl/sdlrun.h"
>                     let eprog = map mkEpic prog
>                     let incs = [(name "hdr", Include sdlh),
>                                 (name "hdr", Include "math.h")]
>                     compileObj (incs ++ sdlPrims ++ 
>                                (name "main", EpicFn runMain):eprog)
>                                 (fp++".o")
>                     linkWith opts [fp++".o", sdlo] fp
