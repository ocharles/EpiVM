> {-# OPTIONS_GHC -XScopedTypeVariables #-}

> module SDLprims where

Epic primitives for calling SDL and basic operators

> import Epic.Epic

> initSDL :: Expr -> Expr -> Term
> initSDL xsize ysize 
>     = foreign_ tyPtr "startSDL" [(xsize, tyInt), (ysize, tyInt)]

> pollEvent   = foreignConst_ tyPtr "pollEvent"
> pressAnyKey = foreignConst_ tyUnit "pressAnyKey"

> flipBuffers :: Expr -> Term
> flipBuffers s = foreign_ tyUnit "flipBuffers" [(s, tyPtr)]

Define some colours, and convert a colour into a tuple of the relevant
RGBA values.

> col_black   = con_ 0
> col_red     = con_ 1
> col_green   = con_ 2
> col_blue    = con_ 3
> col_yellow  = con_ 4
> col_cyan    = con_ 5
> col_magenta = con_ 6
> col_white   = con_ 7

> rgba col = case_ col 
>              [con 0 (tuple_ @@ int 0   @@ int 0   @@ int 0   @@ int 255),
>               con 1 (tuple_ @@ int 255 @@ int 0   @@ int 0   @@ int 255),
>               con 2 (tuple_ @@ int 0   @@ int 255 @@ int 0   @@ int 255),
>               con 3 (tuple_ @@ int 0   @@ int 0   @@ int 255 @@ int 255),
>               con 4 (tuple_ @@ int 255 @@ int 255 @@ int 0   @@ int 255),
>               con 5 (tuple_ @@ int 0   @@ int 255 @@ int 255 @@ int 255),
>               con 6 (tuple_ @@ int 255 @@ int 0   @@ int 255 @@ int 255),
>               con 7 (tuple_ @@ int 255 @@ int 255 @@ int 255 @@ int 255)]

Constants - it's a dynamically typed language so we wrap them in an ADT
which says what type they are.

> mkint i  = con_ 0 @@ i
> mkstr s  = con_ 1 @@ s
> mkchar c = con_ 2 @@ c
> mkbool b = con_ 3 @@ b
> mkcol c  = con_ 4 @@ c

Every time we use a constant, we'll have to extract it from the wrapper.
If we're asking for the wrong type, quit with an error.

ANNOYANCE: Having to add type annotations because we only have Alternative
instances for (Expr -> e). Is there a way to make type inference know that
it must be an Expr because that's the only instance we define? i.e. can
we stop any other instances for (a -> e) being allowed somehow?

> getInt x  = case_ x 
>             [con 0 (\ (x :: Expr) -> x), defaultcase (error_ "Not an Int")]

> getStr x  = case_ x 
>             [con 1 (\ (x :: Expr) -> x), defaultcase (error_ "Not a String")]

> getChar x = case_ x 
>             [con 2 (\ (x :: Expr) -> x), defaultcase (error_ "Not a Char")]

> getBool x = case_ x 
>             [con 3 (\ (x :: Expr) -> x), defaultcase (error_ "Not a Bool")]

> getCol x  = case_ x 
>             [con 4 (\ (x :: Expr) -> x), defaultcase (error_ "Not a Colour")]

Arithmetic operations

> primPlus x y = mkint $ op_ plus_ (getInt x) (getInt y)
> primMinus x y = mkint $ op_ minus_ (getInt x) (getInt y)
> primTimes x y = mkint $ op_ times_ (getInt x) (getInt y)
> primDivide x y = mkint $ op_ divide_ (getInt x) (getInt y)

Comparisons

> primEq x y = mkbool $ op_ eq_ (getInt x) (getInt y)
> primLT x y = mkbool $ op_ lt_ (getInt x) (getInt y)
> primLE x y = mkbool $ op_ lte_ (getInt x) (getInt y)
> primGT x y = mkbool $ op_ gt_ (getInt x) (getInt y)
> primGE x y = mkbool $ op_ gte_ (getInt x) (getInt y)

Graphics primitive, just extracts the tuple of RGBA values for the colour
and calls the SDL_gfx primitive.

> drawLine :: Expr -> Expr -> Expr -> Expr -> Expr -> Expr -> Term
> drawLine surf x y ex ey col
>          = case_ (rgba col)
>              [tuple (\ r g b a ->
>                 foreign_ tyUnit "drawLine" 
>                   [(surf, tyPtr),
>                    (x, tyInt), (y, tyInt), 
>                    (ex, tyInt), (ey, tyInt),
>                    (r, tyInt), (g, tyInt), 
>                    (b, tyInt), (a, tyInt)]) ]

We have integers and degrees, but sin and cos work with floats and radians.
Here's some primitives to do the necessary conversions.

> intToFloat x = foreign_ tyFloat "intToFloat" [(x, tyInt)]
> floatToInt x = foreign_ tyInt "floatToInt" [(x, tyFloat)]

> rad x = op_ timesF_ (intToFloat x) (float (pi/180))

> esin x = foreign_ tyFloat "sin" [(rad x, tyFloat)]
> ecos x = foreign_ tyFloat "cos" [(rad x, tyFloat)]

Turtle functions.
In these, the arguments given by the user are in the Value ADT, so we'll
need to extract the integer.

To move forward, create a new state with the turtle at the new position, 
and draw a line in the current colour between the two positions. 
Return the new state.

> forward :: Expr -> Expr -> Term
> forward st dist = case_ st 
>   [tuple (\ (surf :: Expr) (x :: Expr) (y :: Expr) 
>             (dir :: Expr) (col :: Expr) (pen :: Expr) -> 
>              let_ (op_ plus_ x (floatToInt (op_ timesF_ (intToFloat (getInt dist))
>                                                        (esin dir))))
>              (\ (x' :: Expr) -> let_ (op_ plus_ y (floatToInt 
>                                            (op_ timesF_ (intToFloat (getInt dist))
>                                                        (ecos dir))))
>              (\ (y' :: Expr) -> if_ pen (fn "drawLine" @@ surf @@ x @@ y 
>                                      @@ x' @@ y' @@ col)
>                              unit_ +>
>                      tuple_ @@ surf @@ x' @@ y' @@ dir @@ col @@ pen)))]

To turn right, create a new state with the turtle turned right. 
Return the new state.

> right :: Expr -> Expr -> Term
> right st ang = case_ st
>   [tuple (\ (surf :: Expr) (x :: Expr) (y :: Expr) 
>             (dir :: Expr) (col :: Expr) (pen :: Expr) -> 
>          (tuple_ @@ surf @@ x @@ y @@ op_ minus_ dir (getInt ang) @@ col @@ pen))]

To turn left, create a new state with the turtle turned left. 
Return the new state.

> left :: Expr -> Expr -> Term
> left st ang = case_ st
>   [tuple (\ (surf :: Expr) (x :: Expr) (y :: Expr) 
>             (dir :: Expr) (col :: Expr) (pen :: Expr) -> 
>          (tuple_ @@ surf @@ x @@ y @@ op_ plus_ dir (getInt ang) @@ col @@ pen))]

> colour :: Expr -> Expr -> Term
> colour st col' = case_ st
>   [tuple (\ (surf :: Expr) (x :: Expr) (y :: Expr) 
>             (dir :: Expr) (col :: Expr) (pen :: Expr) -> 
>          (tuple_ @@ surf @@ x @@ y @@ dir @@ getCol col' @@ pen))]

> pen :: Expr -> Expr -> Term
> pen st b = case_ st
>   [tuple (\ (surf :: Expr) (x :: Expr) (y :: Expr) 
>             (dir :: Expr) (col :: Expr) (pen :: Expr) -> 
>          (tuple_ @@ surf @@ x @@ y @@ dir @@ col @@ b))]

Repeat n times

> primRepeat :: Expr -> Expr -> Expr -> Term
> primRepeat st n e = case_ (getInt n)
>                 [constcase 0 st,
>                  defaultcase (let_ (e @@ st)
>                      (\ (st' :: Expr) -> fn "repeat" @@ st'
>                                   @@ mkint (op_ minus_ (getInt n) (int 1))
>                                   @@ e))]

Turtle state consists of an SDL surface,
a position, a direction, a colour, and pen up/down:
(surf, x, y, dir, col, bool)

Note that we use primitives here, not the Value ADT, because we don't allow
the user direct access to this tuple.

> init_turtle surf = tuple_ @@ surf @@ 
>                              int 320 @@ int 240 @@ int 180 @@ 
>                              col_white @@ bool True

Export the primitives as Epic functions.

> sdlPrims = basic_defs ++
>            [EpicFn (name "initSDL") initSDL,
>             EpicFn (name "pollEvent") pollEvent,
>             EpicFn (name "flipBuffers") flipBuffers,
>             EpicFn (name "drawLine") drawLine,
>             EpicFn (name "forward") forward,
>             EpicFn (name "left") left,
>             EpicFn (name "right") right,
>             EpicFn (name "colour") colour,
>             EpicFn (name "pen") pen,
>             EpicFn (name "repeat") primRepeat,
>             EpicFn (name "pressAnyKey") pressAnyKey]
