> module Lexer where

> import Char

> import Turtle

> type Result a = Either (String, FilePath, Int) a

> type LineNumber = Int
> type P a = String -> String -> LineNumber -> Result a
 
> getLineNo :: P LineNumber
> getLineNo = \s fn l -> Right l
 
> getFileName :: P String
> getFileName = \s fn l -> Right fn
 
> getContent :: P String
> getContent = \s fn l -> Right s
 
> thenP :: P a -> (a -> P b) -> P b
> m `thenP` k = \s fn l -> case m s fn l of
>        Right a -> k a s fn l
>        Left (e, f, ln) -> Left (e, f, ln)
 
> returnP :: a -> P a
> returnP a = \s fn l -> Right a
> 
> failP :: String -> P a
> failP err = \s fn l -> Left (err, fn, l)
 
> catchP :: P a -> (String -> P a) -> P a
> catchP m k = \s fn l ->
>    case m s fn l of
>       Right a -> Right a
>       Left (e, f, ln) -> k e s fn l
 
> happyError :: P a
> happyError = reportError "Parse error"
 
> reportError :: String -> P a
> reportError err = getFileName `thenP` \fn ->
>                   getLineNo `thenP` \line ->
>                   getContent `thenP` \content ->
>                       failP (fn ++ ":" ++ show line ++ ":" ++ err ++ " at " ++ take 40 content ++ " ...")
 
> data Token 
>       = TokenName Id
>       | TokenString String
>       | TokenInt Int
>       | TokenChar Char
>       | TokenBool Bool
>       | TokenOB
>       | TokenCB
>       | TokenOCB
>       | TokenCCB
>       | TokenOSB
>       | TokenCSB
>       | TokenPlus
>       | TokenMinus
>       | TokenTimes
>       | TokenDivide
>       | TokenEquals
>       | TokenEQ
>       | TokenGE
>       | TokenLE
>       | TokenGT
>       | TokenLT
>       | TokenLet
>       | TokenIn
>       | TokenIf
>       | TokenThen
>       | TokenElse
>       | TokenRepeat
>       | TokenSemi
>       | TokenComma
>       | TokenMkCol Colour
>       | TokenEval
>       | TokenFD
>       | TokenRight
>       | TokenLeft
>       | TokenColour
>       | TokenPenUp
>       | TokenPenDown
>       | TokenEOF
>  deriving (Show, Eq)
> 
> 
> lexer :: (Token -> P a) -> P a
> lexer cont [] = cont TokenEOF []
> lexer cont ('\n':cs) = \fn line -> lexer cont cs fn (line+1)
> lexer cont (c:cs)
>       | isSpace c = \fn line -> lexer cont cs fn line
>       | isAlpha c = lexVar cont (c:cs)
>       | isDigit c = lexNum cont (c:cs)
>       | c == '_' = lexVar cont (c:cs)
> lexer cont ('"':cs) = lexString cont cs
> lexer cont ('\'':cs) = lexChar cont cs
> lexer cont ('{':'-':cs) = lexerEatComment 0 cont cs
> lexer cont ('-':'-':cs) = lexerEatToNewline cont cs
> lexer cont ('(':cs) = cont TokenOB cs
> lexer cont (')':cs) = cont TokenCB cs
> lexer cont ('{':cs) = cont TokenOCB cs
> lexer cont ('}':cs) = cont TokenCCB cs
> lexer cont ('[':cs) = cont TokenOSB cs
> lexer cont (']':cs) = cont TokenCSB cs
> lexer cont ('+':cs) = cont TokenPlus cs
> lexer cont ('-':cs) = cont TokenMinus cs
> lexer cont ('*':cs) = cont TokenTimes cs
> lexer cont ('/':cs) = cont TokenDivide cs
> lexer cont ('=':'=':cs) = cont TokenEQ cs
> lexer cont ('>':'=':cs) = cont TokenGE cs
> lexer cont ('<':'=':cs) = cont TokenLE cs
> lexer cont ('>':cs) = cont TokenGT cs
> lexer cont ('<':cs) = cont TokenLT cs
> lexer cont ('=':cs) = cont TokenEquals cs
> lexer cont (';':cs) = cont TokenSemi cs
> lexer cont (',':cs) = cont TokenComma cs
> lexer cont (c:cs) = lexError c cs
 
> lexError c s f l = failP (show l ++ ": Unrecognised token '" ++ [c] ++ "'\n") s f l

> lexerEatComment nls cont ('-':'}':cs)
>     = \fn line -> lexer cont cs fn (line+nls)
> lexerEatComment nls cont ('\n':cs) = lexerEatComment (nls+1) cont cs
> lexerEatComment nls cont (c:cs) = lexerEatComment nls cont cs
> 
> lexerEatToNewline cont ('\n':cs)
>    = \fn line -> lexer cont cs fn (line+1)
> lexerEatToNewline cont (c:cs) = lexerEatToNewline cont cs

> lexNum cont cs = case (span isDigit cs) of
>                     (num, rest) ->
>                         cont (TokenInt (read num)) rest

> lexString cont cs =
>    \fn line ->
>    case getstr cs of
>       Just (str,rest,nls) -> cont (TokenString str) rest fn (nls+line)
>       Nothing -> failP (fn++":"++show line++":Unterminated string contant")
>                     cs fn line

> lexChar cont cs =
>    \fn line ->
>    case getchar cs of
>       Just (str,rest) -> cont (TokenChar str) rest fn line
>       Nothing -> 
>           failP (fn++":"++show line++":Unterminated character constant")
>                        cs fn line

> isAllowed c = isAlpha c || isDigit c || c `elem` "_\'?#"

> lexVar cont cs =
>    case span isAllowed cs of
>       ("true",rest) -> cont (TokenBool True) rest
>       ("false",rest) -> cont (TokenBool False) rest
> -- expressions
>       ("let",rest) -> cont TokenLet rest
>       ("if",rest) -> cont TokenIf rest
>       ("then",rest) -> cont TokenThen rest
>       ("else",rest) -> cont TokenElse rest
>       ("repeat",rest) -> cont TokenRepeat rest
>       ("in",rest) -> cont TokenIn rest
>       ("eval",rest) -> cont TokenEval rest
> -- commands
>       ("forward",rest) -> cont TokenFD rest
>       ("right",rest) -> cont TokenRight rest
>       ("left",rest) -> cont TokenLeft rest
>       ("colour",rest) -> cont TokenColour rest
>       ("penup",rest) -> cont TokenPenUp rest
>       ("pendown",rest) -> cont TokenPenDown rest
> -- colours
>       ("black",rest) -> cont (TokenMkCol Black) rest
>       ("red",rest) -> cont (TokenMkCol Red) rest
>       ("green",rest) -> cont (TokenMkCol Green) rest
>       ("blue",rest) -> cont (TokenMkCol Blue) rest
>       ("yellow",rest) -> cont (TokenMkCol Yellow) rest
>       ("cyan",rest) -> cont (TokenMkCol Cyan) rest
>       ("magenta",rest) -> cont (TokenMkCol Magenta) rest
>       ("white",rest) -> cont (TokenMkCol White) rest
>       (var,rest)   -> cont (mkname var) rest
 
> mkname :: String -> Token
> mkname c = TokenName (mkId c)

> getstr :: String -> Maybe (String,String,Int)
> getstr cs = case getstr' "" cs 0 of
>                Just (str,rest,nls) -> Just (reverse str,rest,nls)
>                _ -> Nothing
> getstr' acc ('\"':xs) = \nl -> Just (acc,xs,nl)
> getstr' acc ('\\':'n':xs) = getstr' ('\n':acc) xs -- Newline
> getstr' acc ('\\':'r':xs) = getstr' ('\r':acc) xs -- CR
> getstr' acc ('\\':'t':xs) = getstr' ('\t':acc) xs -- Tab
> getstr' acc ('\\':'b':xs) = getstr' ('\b':acc) xs -- Backspace
> getstr' acc ('\\':'a':xs) = getstr' ('\a':acc) xs -- Alert
> getstr' acc ('\\':'f':xs) = getstr' ('\f':acc) xs -- Formfeed
> getstr' acc ('\\':'0':xs) = getstr' ('\0':acc) xs -- null
> getstr' acc ('\\':x:xs) = getstr' (x:acc) xs -- Literal
> getstr' acc ('\n':xs) = \nl ->getstr' ('\n':acc) xs (nl+1) -- Count the newline
> getstr' acc (x:xs) = getstr' (x:acc) xs
> getstr' _ _ = \nl -> Nothing
 
> getchar :: String -> Maybe (Char,String)
> getchar ('\\':'n':'\'':xs) = Just ('\n',xs) -- Newline
> getchar ('\\':'r':'\'':xs) = Just ('\r',xs) -- CR
> getchar ('\\':'t':'\'':xs) = Just ('\t',xs) -- Tab
> getchar ('\\':'b':'\'':xs) = Just ('\b',xs) -- Backspace
> getchar ('\\':'a':'\'':xs) = Just ('\a',xs) -- Alert
> getchar ('\\':'f':'\'':xs) = Just ('\f',xs) -- Formfeed
> getchar ('\\':'0':'\'':xs) = Just ('\0',xs) -- null
> getchar ('\\':x:'\'':xs) = Just (x,xs) -- Literal
> getchar (x:'\'':xs) = Just (x,xs)
> getchar _ = Nothing
