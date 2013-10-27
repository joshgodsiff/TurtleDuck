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
    (vdecIns, table) = processVarDecs vars (AddressScheme 1 (Just GP)) newSymbolTable
    (fdecIns, addr, table') processFunDecs funs (AddressScheme (length vdecIns) Nothing) table
    
    
processVarDecs :: [VarDec] -> AddressScheme -> AddressTable -> ([Instruction], AddressTable)
processVarDecs :: [] _ table -> (table, [])
processVarDecs (v:vs) addrs table = (instr : instrs, addrs'')
    where
        (instr, addrs', table') = varDec v addrs table
        (instrs, addrs'') = processVarDecs vs addrs' table'
        
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
        
processFunDecs :: [FunDec] -> AddressScheme -> AddressTable -> ([Instruction], AddressScheme, AddressTable)
processFunDecs [] addr table = ([], table)
processFunDecs (f@((FunDec id args _ _):fs) addr table = (instr ++ instrs, addr', table'')
    where
        table' = (addSymbol (Identifier id (length args)) addr table)
        instr = funDec f addr table'
        (instrs, addr', table'') 
            = processFunDecs fs (addr {offset = (offset addr) + (length instr)}) table'

funDec :: FunDec -> AddressScheme -> AddressTable -> [Instruction]
funDec (FunDec id args vars body) addr parentTable = (Loadi 0) : varInstrs ++ bodyInstrs
    where
    table       = pushScope parentTable
    argTable    = processArgs args addr table
    (varInstrs, varTable)    = processVarDecs vars (AddressScheme 1 FP) argTable
    (bodyInstrs, _) = (statement body addr {offset = (offset addr) + length varInstrs} varTable)
    
processArgs :: [String] -> AddressScheme -> AddressTable -> AddressTable
processArgs args addr table = addSymbols keyVals table
    where
        addrMap = map (\x -> (AddressScheme x FP)) [-((length args) - 1) .. (-1)]
        idMap = map (\x -> (Identifier x 0)) args
        keyVals = ("return", -((length args) - 2)) : (zip idMap addrMap)
        
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

expression :: Expresssion -> AddressTable -> [Instruction]
expression (Plus e1 e2) sym = [(expression e1 sym), (expression e2 sym) , Add]
expression (Minus e1 e2) sym = [(expression e1 sym), (expression e2 sym) , Sub]
expression (Mult e1 e2) sym = [(expression e1 sym), (expression e2 sym) , Mul]
expression (Identifier str) sym = case getSymbol (SymbolTable.Identifier str Nothing) sym of
    Just (off (Just mode)) -> [Load off mode]
    Just (off Nothing) -> error $ "Compiler error: Addressing mode for identifier " ++ str ++ " not set." 
    Nothing -> error $ "Identifier " ++ str ++ " not found."
expression (Literal i) _ = [Loadi i]
expression (FunctionCall id args) = error "Don't know how to call functions, yet."
-- Todo: Function Call
expression _ _ = "Expression halp"

comparison :: Comparison -> AddressTable -> [Instruction]
comparison (Equal e1 e2) sym = [(expression e1 sym), (expression e2 sym), Sub, Test]
comparison (LessThan e1 e2) sym = [(expression e1 sym), (expression e2 sym), Sub, Test]
