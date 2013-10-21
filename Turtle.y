{
module Main where

import Lexer (Token)
import qualified Lexer as Tok
}

%name parseTurtle 
%tokentype {Token}
%error {error . show}
%token
    turtle    {Tok.Turtle}
    var       {Tok.Var}
    fun       {Tok.Fun}
    up        {Tok.Up}
    down      {Tok.Down}
    moveto    {Tok.Moveto}
    read      {Tok.Read}
    if        {Tok.If}
    else      {Tok.Else}
    while     {Tok.While}
    return    {Tok.Return}
    '+'       {Tok.Plus}
    '-'       {Tok.Minus}
    '*'       {Tok.Mult}
    '='       {Tok.Assign}
    '('       {Tok.LParen}
    ')'       {Tok.RParen}
    '{'       {Tok.LBrace}
    '}'       {Tok.RBrace}
    ','       {Tok.Comma}
    '<'       {Tok.LessThan}
    '=='      {Tok.Equals}
    identifier {Tok.Identifier $$}
    int       {Tok.Literal $$}

%nonassoc noelse
%nonassoc else
%%

Turtle: turtle identifier VarDecs FunDecs Statement {Turtle $2 $3 $4 $5}

VarDecs : {[]} 
        | var identifier VarAssign VarDecs {VarDec $2 $3 : $4}

VarAssign : {Nothing}
          | '=' Expression {Just $2}

FunDecs : {[]}
        | fun identifier '(' DecArgs VarDecs Statement FunDecs {FunDec $2 $4 $5 $6 : $7}

Statement : Exp {Statement $1}
          | '{' Exp Statement1 {Statements (Statement $2:$3)}

Statement1 : Exp Statement1 {Statement $1:$2}
          | '}' {[]}

Exp : identifier '=' Expression {Assignment $1 $3}
    | IfStmt {$1}
    | while '(' Expression ')' Statement {While $3 $5}
    | read '(' identifier ')' {Read $3}
    | up {Up}
    | down {Down}
    | moveto '(' Expression ',' Expression ')' {MoveTo $3 $5}
    | identifier '(' Args {ExpFunctionCall $1 $3}
    | return Expression {Return $2}
    
IfStmt : MatchedIfStmt {$1}
    | OpenIfStmt {$1}
    
MatchedIfStmt : if '(' Expression ')' Statement else Statement {If $3 $5 (Just $7)}
-- MatchedIfStmt : if '(' Expression ')' MatchedIfStmt else MatchedIfStmt {If $3 $5 (Just $7)}
--               | Statement {$1}

OpenIfStmt : if '(' Expression ')' Statement %prec noelse {If $3 $5 Nothing}
-- OpenIfStmt : if '(' Expression ')' Statement {If $3 $5 Nothing}
--           | if '(' Expression ')' MatchedIfStmt else OpenIfStmt {If $3 $5 (Just $7)}

Expression : Expression '+' Term {Plus $1 $3}
	   | Expression '-' Term {Minus $1 $3}
	   | Expression '==' Term {Equal $1 $3}
	   | Expression '<' Term {LessThan $1 $3}
           | identifier '(' Args {FunctionCall $1 $3}
           | Term {$1}

Term : Term '*' Factor {Mult $1 $3}
     | Factor {$1}

Factor : identifier {Identifier $1}
       | int {Literal $1}
       | '-' Factor {Mult (Literal (-1)) $2}
       | '(' Expression ')' {$2}

DecArgs : ')' {[]}
    | identifier DecArgs1 {$1:$2}

DecArgs1 : ')' {[]}
    | ',' identifier DecArgs1 {$2:$3}

Args : ')' {[]}
     | Expression Args1 {$1:$2}

Args1 : ')' {[]}
      | ',' Expression Args1 {$2:$3}

{ 
-- We build the parse tree so we can check this works
-- this won't be here in the final version

data Turtle = Turtle String [VarDec] [FunDec] Statement
  deriving Show
data VarDec = VarDec String (Maybe Expression)
  deriving Show
data FunDec = FunDec String [String] [VarDec] Statement
  deriving Show
data Statement = Statement Exp
	   | Statements [Statement]
  deriving Show
data Exp = Assignment String Expression
     | If Expression Statement (Maybe Statement)
     | While Expression Statement
     | Read String
     | Up
     | Down
     | MoveTo Expression Expression
     | Return Expression
     | ExpFunctionCall String [Expression]
  deriving Show
data Expression = Plus Expression Expression
	    | Minus Expression Expression
	    | Mult Expression Expression
	    | Literal Int
	    | Identifier String
	    | Equal Expression Expression
	    | LessThan Expression Expression
	    | FunctionCall String [Expression]
  deriving Show

main = getContents >>= print . parseTurtle . Tok.alexScanTokens
}

