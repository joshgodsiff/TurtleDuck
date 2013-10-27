-- | Module for doing base conversions from Turtle to PDPlot
-- by Robert 'Probie' Offner and Joshua Godsiff

module Base where

import PDPlot
import TurtleData
import SymbolTable

data AddressScheme = AddressScheme {offset :: Int, from :: Maybe TargetPointer}
    deriving Show
type AddressTable  = SymbolTable AddressScheme

turtle :: Turtle -> [Instruction]
turtle (Turtle name vars funs sts) = vdecIns ++ fdecIns ++ stmtIns
    where
    (vdecIns, table) = processVarDecs vars (AddressScheme 1 (Just GP)) newSymbolTable
    (fdecIns, addr, table') = processFunDecs funs (AddressScheme (length vdecIns) Nothing) table
    (stmtIns, _) = statement sts (addr {offset = offset addr + 1}) table'

processVarDecs :: [VarDec] -> AddressScheme -> AddressTable -> ([Instruction], AddressTable)
processVarDecs [] _ table = ([], table)
processVarDecs (v:vs) addrs table = (instr ++ instrs, addrs'')
    where
        (instr, addrs', table') = varDec v addrs table
        (instrs, addrs'') = processVarDecs vs addrs' table'
        
varDec :: VarDec -> AddressScheme -> AddressTable -> ([Instruction], AddressScheme, AddressTable)
varDec (VarDec ident Nothing) address sym 
    =   ([Loadi 0]
         , address {offset = (offset address) + 1}
         , (addSymbol (SymbolTable.Identifier ident Nothing) address) sym)
varDec (VarDec ident (Just e)) address sym 
    =   (expression e sym -- need to make sure that 'expression e' leaves its value in the right spot?
         , address {offset = (offset address) + 1}
         , (addSymbol (SymbolTable.Identifier ident Nothing) address sym))
-- varDec _ _ _ = error "VarDec Halp"
        
processFunDecs :: [FunDec] -> AddressScheme -> AddressTable -> ([Instruction], AddressScheme, AddressTable)
processFunDecs [] addr table = ([], addr, table)
processFunDecs (f@(FunDec id args _ _):fs) addr table = (instr ++ instrs, addr', table'')
    where
        table' = addSymbol (SymbolTable.Identifier id (Just (length args))) addr table
        instr = funDec f addr table'
        (instrs, addr', table'') 
            = processFunDecs fs (addr {offset = (offset addr) + (length instr)}) table'

funDec :: FunDec -> AddressScheme -> AddressTable -> [Instruction]
funDec (FunDec id args vars body) addr parentTable = varInstrs ++ bodyInstrs ++ [Rts]
    where
    table       = pushScope parentTable
    argTable    = processArgs args addr table
    (varInstrs, varTable)    = processVarDecs vars (AddressScheme 1 (Just FP)) argTable
    (bodyInstrs, _) = (statement body addr {offset = (offset addr) + length varInstrs} varTable)
    
processArgs :: [String] -> AddressScheme -> AddressTable -> AddressTable
processArgs args addr table = addSymbols keyVals table
    where
        addrMap = map (\x -> (AddressScheme x (Just FP))) [(-(length args) - 1) .. (-1)]
        idMap = map (\x -> (SymbolTable.Identifier x Nothing)) args
        keyVals 
            = (SymbolTable.Identifier "return" Nothing, 
            AddressScheme (-(length args) - 2) (Just FP))
            : (zip idMap addrMap)
        
exp :: Exp -> AddressScheme -> AddressTable -> [Instruction]
exp TurtleData.Up _ s       = [PDPlot.Up]
exp TurtleData.Down _ s     = [PDPlot.Down]
exp (MoveTo e1 e2) _ sym    = (expression e1 sym) ++ (expression e2 sym) ++ [Move]
exp (If cond thenBlock elseBlock) addr sym
    = preThen ++ thenInstr ++ [Jump $ Left $ fromIntegral $ offset addr''] ++ elseInstr
    where
        condInstr = comparison cond sym
        preThen = condInstr ++ thenJump ++ [Jump $ Left $ fromIntegral $ offset addr'] -- +1?
        thenStart = (addr {offset = (offset addr) + (length preThen)})
        (thenInstr, addr')
            = statement thenBlock thenStart sym
        (elseInstr, addr'') = case elseBlock of
            Nothing -> ([], addr')
            Just s -> (statement s (addr' {offset = (offset addr') + (length thenInstr) + 1}) sym)
        thenJump = case cond of
            (Equal _ _) -> [Jeq $ Left $ fromIntegral $ offset thenStart]
            (LessThan _ _) -> [Jlt $ Left $ fromIntegral $ offset thenStart]
exp (While cond codeBlock) addr sym
    = preLoop ++ loopBody ++ [Jump $ Left $ fromIntegral $ offset addr]
    where
        condInstr = comparison cond sym
        preLoop = condInstr ++ loopJump ++ [Jump $ Left $ fromIntegral $ offset addr' + 2] -- The jump statement is two words
        loopStart = (addr {offset = (offset addr) + (length preLoop)})
        (loopBody, addr') = statement codeBlock loopStart sym 
        loopJump = case cond of
            (Equal _ _) -> [Jeq $ Left $ fromIntegral $ offset loopStart]
            (LessThan _ _) -> [Jlt $ Left $ fromIntegral $ offset loopStart]
exp (TurtleData.Read str) addr sym = case getSymbol (SymbolTable.Identifier str Nothing) sym of
     Just (AddressScheme off (Just mode)) -> [PDPlot.Read (fromIntegral off) mode]
     Just (AddressScheme off Nothing) 
         -> error $ "Compiler error: Addressing mode for identifier " ++ str ++ " not set." 
     Nothing -> error $ "Identifier " ++ str ++ " not found."
exp (Assignment id e) addr sym = case getSymbol (SymbolTable.Identifier id Nothing) sym of
    Nothing -> error $ "Identifier " ++ id ++ " not found."
    Just (AddressScheme off (Just pointer)) -> eInstr ++ [Store (fromIntegral off) pointer]
        where
            eInstr = (expression e sym)
    Just (AddressScheme off Nothing)
        -> error $ "Error: Variable " ++ id ++ " has an address in static memory"
-- Same as the Expression version. Should merge them somehow.
exp (ExpFunctionCall id args) addr sym = (Loadi 0) : argIns ++ [maybeFn, Pop $ fromIntegral $ length args + 1]
    where
        argIns = concatMap (flip expression sym) args
        maybeFn = case getSymbol (SymbolTable.Identifier id (Just $ length args)) sym of
            Nothing -> Jsr $ Right $ (SymbolTable.Identifier id (Just $ length args))
            Just (AddressScheme addr Nothing) -> Jsr $ Left $ fromIntegral addr
            Just (AddressScheme addr (Just foo))
                -> error "Something when horribly wrong trying to address a function."
exp (Return e) addr sym = eInstr ++ [Store (fromIntegral returnAddr) FP, Rts]
    where
        returnID =  getSymbol (SymbolTable.Identifier "return" Nothing) sym
        returnAddr = case returnID of
            Nothing -> error "Compiler Error: Function does not have a return address (wot?!)"
            Just (AddressScheme off (Just FP)) -> off
            _ -> error "Compiler Error: Function has bad return address."
        eInstr = expression e sym

-- Pretty sure Statements can't change the address table? So don't need to return it.
statement :: Statement -> AddressScheme -> AddressTable -> ([Instruction], AddressScheme)
statement (Statement e) addr table          = (eInstr, addr {offset = (offset addr) + length eInstr})
    where eInstr = Base.exp e addr table
statement (Statements (s:ss)) addr table    = (sInstr ++ ssInstr, addr'')
    where 
        (sInstr, addr')     = statement s addr table
        (ssInstr, addr'')   = statement (Statements ss) addr' table
statement (Statements []) addr _            = ([], addr)

expression :: Expression -> AddressTable -> [Instruction]
expression (Plus e1 e2) sym         = (expression e1 sym) ++ (expression e2 sym) ++ [Add]
expression (Minus e1 e2) sym        = (expression e1 sym) ++ (expression e2 sym) ++ [Sub]
expression (Mult e1 e2) sym         = (expression e1 sym) ++ (expression e2 sym) ++ [Mul]
expression (TurtleData.Identifier str) sym     = case getSymbol (SymbolTable.Identifier str Nothing) sym of
    Just (AddressScheme off (Just mode)) -> [Load (fromIntegral off) mode]
    Just (AddressScheme off Nothing) 
        -> error $ "Compiler error: Addressing mode for identifier " ++ str ++ " not set." 
    Nothing -> error $ "Error in parsing identifier expression: Identifier " ++ str ++ " not found."
expression (Literal i) _            = [Loadi (fromIntegral i)]
-- TODO: Return result.
expression (FunctionCall id args) sym  = (Loadi 0) : argIns ++ [maybeFn, Pop $ fromIntegral $ length args]
    where
        argIns = concatMap (flip expression sym) args
        maybeFn = case getSymbol (SymbolTable.Identifier id (Just $ length args)) sym of
            Nothing -> Jsr $ Right $ (SymbolTable.Identifier id (Just $ length args))
            Just (AddressScheme addr Nothing) -> Jsr $ Left $ fromIntegral addr
            Just (AddressScheme addr (Just foo))
                -> error "Something when horribly wrong trying to address a function."
-- Todo: Function Call
-- expression _ _ = error "Expression halp"

comparison :: Comparison -> AddressTable -> [Instruction]
comparison (Equal e1 e2) sym = (expression e1 sym) ++ (expression e2 sym) ++ [Sub, Test, (Pop 1)]
comparison (LessThan e1 e2) sym = (expression e1 sym) ++ (expression e2 sym) ++ [Sub, Test, (Pop 1)]
