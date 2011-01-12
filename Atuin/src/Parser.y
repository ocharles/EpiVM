{ -- -*-Haskell-*-
{-# OPTIONS_GHC -fglasgow-exts #-}

module Parser where

import Char
import Turtle
import Lexer

}

%name mkparse Program

%tokentype { Token }
%monad { P } { thenP } { returnP }
%lexer { lexer } { TokenEOF }

%token 
      name            { TokenName $$ }
      string          { TokenString $$ }
      char            { TokenChar $$ }
      int             { TokenInt $$ }
      bool            { TokenBool $$ }
      col             { TokenMkCol $$ }
      let             { TokenLet }
      in              { TokenIn }
      if              { TokenIf }
      then            { TokenThen }
      else            { TokenElse }
      repeat          { TokenRepeat }
      '('             { TokenOB }
      ')'             { TokenCB }
      '{'             { TokenOCB }
      '}'             { TokenCCB }
      '['             { TokenOSB }
      ']'             { TokenCSB }
      '+'             { TokenPlus }
      '-'             { TokenMinus }
      '*'             { TokenTimes }
      '/'             { TokenDivide }
      '='             { TokenEquals }
      eq              { TokenEQ }
      le              { TokenLE }
      ge              { TokenGE }
      '<'             { TokenLT }
      '>'             { TokenGT }
      ';'             { TokenSemi }
      ','             { TokenComma }
      eval            { TokenEval }
      forward         { TokenFD }
      right           { TokenRight }
      left            { TokenLeft }
      colour          { TokenColour }
      penup           { TokenPenUp }
      pendown         { TokenPenDown }

%nonassoc NONE
%left eq 
%left ';'
%left '<' '>' le ge 
%left '+' '-' 
%left '*' '/' 
%left NEG

%%

Program :: { [(Id, Function)] }
Program : Function { [$1] }
        | Function Program { $1:$2 }

Function :: { (Id, Function) }
Function : name '(' Vars ')' Block { ($1, ($3, $5)) }

Vars :: { [Id] }
Vars : { [] }
     | name { [$1] }
     | name ',' Vars { $1:$3 }

TurtleProg :: { Turtle }
TurtleProg : Turtle { $1 }
           | Turtle TurtleProg { Seq $1 $2 }
           | name '=' Expr TurtleProg { Let $1 $3 $4 }

Block :: { Turtle }
Block : '{' TurtleProg '}' { $2 }
      | Turtle { $1 }

Turtle :: { Turtle }
Turtle : name '(' ExprList ')' { Call $1 $3 }
       | if Expr Block ElseBlock
               { If $2 $3 $4 }
       | eval Expr { Eval $2 }
       | repeat Expr Block { Repeat $2 $3 }
       | forward Expr { Turtle (Fd $2) }
       | right Expr { Turtle (RightT $2) }
       | left Expr { Turtle (LeftT $2) }
       | colour Expr { Turtle (Colour $2) }
       | penup { Turtle PenUp }
       | pendown { Turtle PenDown }

ElseBlock :: { Turtle }
ElseBlock : { Pass }
          | else Block { $2 }

ExprList :: { [Exp] }
ExprList : Expr { [$1] }
         | Expr ',' ExprList { $1:$3 }

Expr :: { Exp }
Expr : name { Var $1 }
     | Constant { Const $1 }
     | '-' Expr %prec NEG { Infix Minus (Const (MkInt 0)) $2 }
     | Expr '+' Expr { Infix Plus $1 $3 }
     | Expr '-' Expr { Infix Minus $1 $3 }
     | Expr '*' Expr { Infix Times $1 $3 }
     | Expr '/' Expr { Infix Divide $1 $3 }
     | Expr eq Expr  { Infix Eq $1 $3 }
     | Expr '<' Expr { Infix Lt $1 $3 }
     | Expr '>' Expr { Infix Gt $1 $3 }
     | Expr le Expr  { Infix Le $1 $3 }
     | Expr ge Expr  { Infix Ge $1 $3 }
     | '(' Expr ')'  { $2 }
     | '{' TurtleProg '}' { Block $2 }

Constant :: { Const }
Constant : int    { MkInt $1 }
         | string { MkString $1 }
         | char   { MkChar $1 }
         | bool   { MkBool $1 }
         | col    { MkCol $1 }

Line :: { LineNumber }
     : {- empty -}      {% getLineNo }

File :: { String } 
     : {- empty -} %prec NONE  {% getFileName }

{

parse :: String -> FilePath -> Result [(Id, Function)]
parse s fn = mkparse s fn 1

parseFile :: FilePath -> IO (Result [(Id, Function)])
parseFile fn = do s <- readFile fn
                  let x = parse s fn
                  return x

}