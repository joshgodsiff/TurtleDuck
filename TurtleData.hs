-- | Module for storing data structures used to represent Turtle syntax.
-- by Robert 'Probie' Offner and Joshua Godsiff

module TurtleData 
    ( Turtle (..)
    , VarDec(..) 
    , FunDec (..)
    , Statement (..)
    , Exp (..)
    , Expression (..)
    ) where

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
     | If Comparison Statement (Maybe Statement)
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
	    | FunctionCall String [Expression]
  deriving Show
  
data Comparison = Equal Expression Expression
        | LessThan Expression Expression