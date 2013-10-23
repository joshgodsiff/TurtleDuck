-- | Module for doing base conversions from Turtle to PDPlot
-- by Robert 'Probie' Offner and Joshua Godsiff

module Base where

import PDPlot
import TurtleData
import SymbolTable

type AddressScheme = AddressScheme (offset :: Int, from :: Maybe TargetPointer)
type AddressTable  = SymbolTable AddressScheme

turtle :: Turtle -> [Instruction]
turtle (Turtle name vars funs sts) = 
    (vdecIns, table) = processVarDecs vars (AddressScheme 1 GP) newSymbolTable
    
processVarDecs :: [VarDec] -> AddressScheme -> AddressTable -> ([Instruction], AddressTable)
processVarDecs :: [] _ table -> (table, [])
processVarDecs (v:vs) addrs table = (instr : instrs, addrs'')
    where
        (instr, addrs', table') = varDec v addrs table
        (instrs, addrs'') = processVarDecs vs addrs' table'

exp :: Exp -> AddressScheme -> AddressTable -> (AddressTable, [Instruction])
exp TurtleData.Up _ s = (s, [PDPlot.Up])
exp TurtleData.Down _ s = (s, [PDPlot.Down])
exp (MoveTo e1 e2) _ sym = ([(expression e1 sym), (expression e2 sym), Move], sym)
exp (If cond thenBlock elseBlock) addr sym = 
    where
        -- Will need to get length of this block, to know address offsets.
        thenInstr = ???
        condInstr = comparison cond sym
exp (While cond codeBlock) sym = error "While Halp"
exp (Read str) sym = error "Read Halp"
exp _ _ = error "Exp Halp"

-- Pretty sure Statements can't change the address table? So don't need to return it.
statement :: Statement -> AddressScheme -> AddressTable -> ([Instruction], AddressScheme)
statement (Statement e) addr table = (eInstr, addr (offset = (offset addr) + length eInstr))
    where eInstr = expression e table
statement (Statements (s:ss)) addr table = (sInstr : ssInstr, addr'')
    where 
        (sInstr, addr') = statement s addr table
        (ssInstr, addr'') = statement ss addr' table
statement (Statements []) addr _ = ([], addr)

varDec :: VarDec -> AddressScheme -> AddressTable -> ([Instruction], AddressScheme, AddressTable)
varDec (VarDec ident Nothing) address sym 
    =   ([Loadi 0], 
        (addSymbol (Identifier ident Nothing) address),
         address {offset = (offset address) + 1)
varDec (VarDec ident (Just e)) address sym 
    =   (expression e sym, -- need to make sure that 'expression e' leaves its value in the right spot?
        (addSymbol (Identifier ident Nothing) address), 
         address {offset = (offset address) + 1))
varDec _ _ = error "VarDec Halp"


expression :: Expresssion -> AddressTable -> [Instruction]
expression (Plus e1 e2) sym = [(expression e1 sym), (expression e2 sym) , Add]
expression (Minus e1 e2) sym = [(expression e1 sym), (expression e2 sym) , Sub]
expression (Mult e1 e2) sym = [(expression e1 sym), (expression e2 sym) , Mul]
expression (Identifier str) sym = case getSymbol (SymbolTable.Identifier sym Nothing) sym of
    Just 

-- Todo: Literal, Identifier, Function Call
expression _ _ = "Expression halp"

comparison :: Comparison -> AddressTable -> [Instruction]
comparison (Equal e1 e2) sym = [(expression e1 sym), (expression e2 sym), Sub, Test]
comparison (LessThan e1 e2) sym = [(expression e1 sym), (expression e2 sym), Sub, Test]
