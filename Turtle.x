-- | Lexer Module
-- by Robert 'Probie' Offner and Josh Godsiff (u4849459 and u4685222)

{
module Lexer where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
    "//".*      ;
    "turtle"    {const Turtle}
    "var"       {const Var}
    "fun"       {const Fun}
    "up"        {const Up}
    "down"      {const Down}
    "moveto"    {const Moveto}
    "read"      {const Read}
    "if"        {const If}
    "else"      {const Else}
    "while"     {const While}
    "return"    {const Return}
    "+"         {const Plus}
    "-"         {const Minus}
    "*"         {const Mult}
    "="         {const Assign}
    "("         {const LParen}
    ")"         {const RParen}
    "{"         {const LBrace}
    "}"         {const RBrace}
    ","         {const Comma}
    "<"         {const LessThan}
    "=="        {const Equals}
    $alpha[$alpha $digit \_ \']* {Identifier}
    $digit+     {Literal . read}
    $white+     ;

{
data Token = Turtle
           | Var
           | Fun
           | Up
           | Down
           | Moveto
           | Read
           | If
           | Else
           | While
           | Return
           | Plus
           | Minus
           | Mult
           | Assign
           | LParen
           | RParen
           | LBrace
           | RBrace
           | Comma
           | LessThan
           | Equals
           | Identifier String
           | Literal Int
    deriving Show
}
