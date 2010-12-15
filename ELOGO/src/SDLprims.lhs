> {-# OPTIONS_GHC -fglasgow-exts #-}

> module SDLprims where

Epic primitives for calling SDL

> import Epic.Epic

> initSDL :: Expr -> Expr -> Term
> initSDL xsize ysize 
>     = foreign_ tyPtr "startSDL" [(xsize, tyInt), (ysize, tyInt)]

> pollEvent   = foreignConst_ tyPtr "pollEvent"
> pressAnyKey = foreignConst_ tyUnit "pressAnyKey"

> flipBuffers :: Expr -> Term
> flipBuffers s = foreign_ tyUnit "flipBuffers" [(s, tyPtr)]

Convert a colour into a tuple of the relevant RGBA values

> rgba :: Expr -> Term
> rgba col = case_ col 
>              [con 0 (tuple_ @@ int 0 @@ int 0 @@ int 0 @@ int 255),
>               con 1 (tuple_ @@ int 255 @@ int 0 @@ int 0 @@ int 255),
>               con 2 (tuple_ @@ int 0 @@ int 255 @@ int 0 @@ int 255),
>               con 3 (tuple_ @@ int 0 @@ int 0 @@ int 255 @@ int 255),
>               con 4 (tuple_ @@ int 255 @@ int 255 @@ int 0 @@ int 255),
>               con 5 (tuple_ @@ int 0 @@ int 255 @@ int 255 @@ int 255),
>               con 6 (tuple_ @@ int 255 @@ int 0 @@ int 255 @@ int 255),
>               con 7 (tuple_ @@ int 255 @@ int 255 @@ int 255 @@ int 255)]

> drawLine :: Expr -> -- surface
>             Expr -> Expr -> Expr -> Expr -> -- x, y, endx, endy
>             Expr -> -- colour
>             Term
> drawLine surf x y ex ey col
>          = case_ (rgba col)
>              [tuple (\ r g b a ->
>                 foreign_ tyUnit "drawLine" 
>                   [(surf, tyPtr),
>                    (x, tyInt), (y, tyInt), (ex, tyInt), (ey, tyInt),
>                    (r, tyInt), (g, tyInt), (b, tyInt), (a, tyInt)]) ]

> col_black   = con_ 0
> col_red     = con_ 1
> col_green   = con_ 2
> col_blue    = con_ 3
> col_yellow  = con_ 4
> col_cyan    = con_ 5
> col_magenta = con_ 6
> col_white   = con_ 7

We have integers and degrees, but sin and cos work with floats and radians.
Here's some primitives to do the necessary conversions.

> intToFloat x = foreign_ tyFloat "intToFloat" [(x, tyInt)]
> floatToInt x = foreign_ tyInt "floatToInt" [(x, tyFloat)]

> rad x = op_ FTimes (intToFloat x) (float (pi/180))

> esin x = foreign_ tyFloat "sin" [(rad x, tyFloat)]
> ecos x = foreign_ tyFloat "cos" [(rad x, tyFloat)]

Create a new state with the turtle at the new position, and draw a line
in the current colour between the two positions. Return the new state.

> forward st dist = case_ st 
>   [tuple (\ (surf :: Expr) (x :: Expr) (y :: Expr) 
>             (dir :: Expr) (col :: Expr) (pen :: Expr) -> 
>              let_ (op_ Plus x (floatToInt (op_ FTimes (intToFloat dist)
>                                                        (esin dir))))
>              (\x' -> let_ (op_ Plus y (floatToInt 
>                                            (op_ FTimes (intToFloat dist)
>                                                        (ecos dir))))
>              (\y' -> drawLine surf x y x' y' col +>
>                      tuple_ @@ surf @@ x' @@ y' @@ dir @@ col @@ pen)))]

Create a new state with the turtle turned right. Return the new state.

> right st ang = case_ st
>   [tuple (\ (surf :: Expr) (x :: Expr) (y :: Expr) 
>             (dir :: Expr) (col :: Expr) (pen :: Expr) -> 
>          (tuple_ @@ surf @@ x @@ y @@ (op_ Minus dir ang) @@ col @@ pen))]

Create a new state with the turtle turned left. Return the new state.

> left st ang = case_ st
>   [tuple (\ (surf :: Expr) (x :: Expr) (y :: Expr) 
>             (dir :: Expr) (col :: Expr) (pen :: Expr) -> 
>          (tuple_ @@ surf @@ x @@ y @@ (op_ Plus dir ang) @@ col @@ pen))]

Turtle state consists of an SDL surface,
a position, a direction, a colour, and pen up/down:
(surf, x, y, dir, col, bool)

> init_turtle surf = tuple_ @@ surf @@ 
>                              int 320 @@ int 240 @@ int 180 @@ 
>                              col_white @@ bool True

> sdlPrims = [(name "initSDL",     EpicFn initSDL),
>             (name "pollEvent",   EpicFn pollEvent),
>             (name "flipBuffers", EpicFn flipBuffers),
>             (name "drawLine",    EpicFn drawLine),
>             (name "pressAnyKey", EpicFn pressAnyKey)]
