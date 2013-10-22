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
	    | Equal Expression Expression -- Should the comparators be in a different part of the grammar? We don't want variables being able to be set to them.
	    | LessThan Expression Expression
	    | FunctionCall String [Expression]
  deriving Show