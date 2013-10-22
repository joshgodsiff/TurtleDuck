-- | Module for doing base conversions from Turtle to PDPlot
-- by Robert 'Probie' Offner and Joshua Godsiff

module Base where

import PDPlot
import TurtleData
import SymbolTable

exp :: Exp -> SymbolTable a -> (SymbolTable a, [Instruction])
exp TurtleData.Up s = ( (s, [PDPlot.Up])
exp TurtleData.Down s = (s, [PDPlot.Down])
exp (MoveTo e1 e2) sym = (sym, [(expression e1 sym), (expression e2 sym), Move]
exp _ _ = error "Exp Halp"

-- Address is the base offset for whatever scope.
varDec :: VarDec -> Address -> SymbolTable a -> (SymbolTable a, [Instruction])
varDec (VarDec ident Nothing) address sym = ((addSymbol (Identifier ident Nothing) address), [Loadi 0])
varDec (VarDec ident (Just e)) address sym = ((addSymbol (Identifier ident Nothing) address), 
                                              expression e sym) -- need to make sure that 'expression e' leaves its value in the right spot?
varDec _ _ = error "VarDec Halp"


expression :: Expresssion -> SymbolTable a -> [Instruction]
expression (Plus e1 e2) sym = [(expression e1 sym), (expression e2 sym) , Add]
expression (Minus e1 e2) sym = [(expression e1 sym), (expression e2 sym) , Sub]
expression (Mult e1 e2) sym = [(expression e1 sym), (expression e2 sym) , Mul]
-- Should possibly do something different/extra for comparisons?
expression (Equal e1 e2) sym = [(expression e1 sym), (expression e2 sym), Sub, Test]
expression (LessThan e1 e2) sym = [(expression e1 sym), (expression e2 sym), Sub, Test]
-- Todo: Literal, Identifier, Function Call
expression _ _ = "Expression halp"