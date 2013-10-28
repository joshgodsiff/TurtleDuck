{-# Language Rank2Types, NoMonomorphismRestriction #-}
-- | A clean module version of base #todo real intro
-- by Robert 'Probie' Offner and Josh Godsiff

module TurtleBase where

import qualified PDPlot as P
import qualified SymbolTable as Sym
import qualified TurtleData as T
import qualified Data.Map as M
import Data.Int
import Control.Monad
import Data.Maybe
import Data.Array

data AddressScheme = AddressScheme {offset :: Int16, from :: P.TargetPointer}
                   | LinkLater 
    deriving Show

data Pointers = Pointer Int16 Int16
    
data TurtleState = TurtleState (DList P.Instruction)
    (Sym.SymbolTable AddressScheme) (P.LookupTable) Int16 Pointers

-- The next few lines are just to keep dependencies down

type DList a = [a] -> [a]

append :: a -> DList a -> DList a
append x xs =  xs . (x:)

-- This is really just an instance of the state monad, but in the interests
-- of keeping dependencies down
data TurtleCompilation a = TurtleCompilation
    {runTurtleCompilation :: TurtleState -> (a, TurtleState)}

-- Usual boilerplate
instance Functor TurtleCompilation where
    fmap f x = TurtleCompilation $ \s -> let (x', s') = runTurtleCompilation x s
                                         in (f x', s')

instance Monad TurtleCompilation where
    return x = TurtleCompilation $ \s -> (x,s)
    x >>= f = join_ (fmap f x)
        where join_ (TurtleCompilation x) = TurtleCompilation $
                  \s -> let (TurtleCompilation x', s') = x s
                        in x' s'

-- Even SPJ thinks lenses are all the rage. I myself am somewhat
-- partial to van Laarhoven lenses since they work with
-- the standard Haskell function composition
type Lens a b = forall f . Functor f => (b -> f b) -> a -> f a

-- The const functor is the getter
newtype Getting b a = Getting { got :: b }
instance Functor (Getting b) where
    fmap _ (Getting b) = Getting b

infixl 8 ^.
x ^. l = got (l Getting x)

-- The trivial functor is the setter.
newtype Setting a = Setting { unsetting :: a }
instance Functor Setting where
    fmap f (Setting a) = Setting (f a)

-- Function application in a lens
infixr 4 %=
l %= f = unsetting . l (Setting . f)

-- Assignment in a lens
infixr 4 ^=
l ^= v = l %= const v

-- And probably the most important thing; the way to build a lens
lens f g h x = fmap (g x) (h (f x))

-- And now to rapidly define some things to make a syntax perhaps more
-- intuitive to my team member (just pretend the . isn't there) XD

infixr 4 .=
(.=) :: Lens TurtleState b -> b -> TurtleCompilation ()
l .= x = TurtleCompilation $ \s -> ((), (l ^= x) s)

infixr 4 .$
(.$) :: Lens TurtleState b -> (b -> b) -> TurtleCompilation ()
l .$ f = TurtleCompilation $ \s -> ((), (l %= f) s)

infixr 4 .+=
(.+=) :: (Num a) => Lens TurtleState a -> a -> TurtleCompilation ()
l .+= x = l .$ (+ x)

-- Now for some helpful lenses (think variables for accessing the
-- internal state of the TurtleCompilation)
-- I toyed with the idea of using template Haskell for this, but
-- decided against it (and regret it)

instructionList = lens (\(TurtleState x _ _ _ _ ) -> x) (\(TurtleState x a b c d) x' -> TurtleState x' a b c d)

functionTable = lens (\(TurtleState _ _ x _ _) -> x) (\(TurtleState a b x c d) x' -> TurtleState a b x' c d)

symbolTable = lens (\(TurtleState _ x _ _ _) -> x) (\(TurtleState a x b c d) x' -> TurtleState a x' b c d)

currentAddress = lens (\(TurtleState _ _ _ x _) -> x) (\(TurtleState a b c x d) x' -> TurtleState a b c x' d)

pointer = lens (\(TurtleState _ _ _ _ x) -> x) (\(TurtleState a b c d x) x' -> TurtleState a b c d x')

global = lens (\(Pointer x _) -> x) (\(Pointer x a) x' -> Pointer x' a)

local = lens (\(Pointer _ x) -> x) (\(Pointer a x) x' -> Pointer a x')

-- Look up a symbol in the symbol table (unsafe if used improperly)
symbol x = lens (\sym -> fromJust $ Sym.getSymbol x sym) (\sym x' -> Sym.addSymbol x x' sym) 

valueOf :: Lens TurtleState a -> TurtleCompilation a
valueOf l = TurtleCompilation $ \s -> (s ^. l, s)

emit :: [P.Instruction] -> TurtleCompilation ()
emit instructions = forM_ instructions $ \instruction -> do
    instructionList .$ append instruction
    currentAddress .+= P.instructionLength instruction

runTurtle :: T.Turtle -> [P.Instruction]
runTurtle t = (snd (runTurtleCompilation (turtle t)
    (TurtleState id Sym.newSymbolTable M.empty 0 (Pointer 0 0))) ^. instructionList) []
    
turtle :: T.Turtle -> TurtleCompilation ()
turtle (T.Turtle name vars funs stmts) = do
    -- TODO: Jump to main
    getFunctionNames funs
    compileGlobalVariables vars
    endOfGlobals <- valueOf currentAddress
    emit [P.Jump (Left 0)]
    compileFunctions funs
    endOfFunctions <- valueOf currentAddress
    backpatch endOfGlobals (P.Jump (Left endOfFunctions))
    compileStatements stmts
<<<<<<< HEAD
    emit [P.Halt]

backpatch :: Int16 -> P.Instruction -> TurtleCompilation ()
backpatch pos instr = instructionList .$ (backpatch' pos instr .)

backpatch' :: Int16 -> P.Instruction -> [P.Instruction] -> [P.Instruction]
backpatch' _ n [] = error $ "Oh shit we fucked up the backpatch " ++ show n ++ " still to go"
backpatch' n _ _ | n < 0 = error "Oh shit we fucked up the backpatch; they don't add up"
backpatch' 0 y (x:xs) = (y:xs)
backpatch' n y (x:xs) = x: (backpatch' (n - P.instructionLength x) y xs)
=======
    emit [Halt] -- Is it necessary to increment current address after this?
>>>>>>> 2b6f131964caf249c5b9b442ebdd1966fb224b28

getFunctionNames :: [T.FunDec] -> TurtleCompilation ()
getFunctionNames funs = forM_ funs $
    \(T.FunDec name _ args _) -> do
        symbolTable.symbol (Sym.Identifier name (Just (length args))) .= LinkLater 

compileGlobalVariables :: [T.VarDec] -> TurtleCompilation ()
compileGlobalVariables decs = forM_ decs $
    \(T.VarDec ident def) -> do
        case def of
           Nothing -> do
               emit [P.Loadi 0]
           Just x -> compileExpression x
        varAddr <- valueOf (pointer.global)
        symbolTable.symbol (Sym.Identifier ident Nothing) .= AddressScheme varAddr P.GP
        pointer.global .+= 1
        
compileFunctions :: [T.FunDec] -> TurtleCompilation ()
compileFunctions funs = forM_ funs $
   \(T.FunDec id args vars body) -> do
        funAddr <- valueOf currentAddress
        symbolTable.symbol (Sym.Identifier id (Just (length args))) .= AddressScheme funAddr P.PC
        symbolTable .$ Sym.pushScope
        compileArgs args
        compileLocalVariables vars
        compileStatements body
        symbolTable .$ Sym.popScope
        emit [P.Rts]
        return () 
       
compileArgs :: [String] -> TurtleCompilation ()
compileArgs args = forM_ (zip [-(fromIntegral $ length args) - 1..] args) $
    \(pos, arg) -> symbolTable.symbol (Sym.Identifier arg Nothing) .= AddressScheme pos P.FP

compileLocalVariables :: [T.VarDec] -> TurtleCompilation ()
compileLocalVariables decs = forM_ (zip [1..] decs) $
    \(val, T.VarDec ident def) -> do
        case def of
           Nothing -> do
               emit [P.Loadi 0]
           Just x -> compileExpression x
        symbolTable.symbol (Sym.Identifier ident Nothing) .= AddressScheme val P.FP
        
compileStatements (T.Statement exp) = compileExp exp
compileStatements (T.Statements stmts) = mapM_ compileStatements stmts

compileExp T.Up = do
    emit [P.Up]
compileExp T.Down = do
    emit [P.Down]
compileExp (T.MoveTo x y) = do
    compileExpression x
    compileExpression y
    emit [P.Move]
compileExp (T.Read x) = do
    -- TODO: Throw error on undeclared x
    (AddressScheme addr from) <- valueOf (symbolTable.symbol(Sym.Identifier x Nothing))
    emit [P.Read (fromIntegral addr) from]
<<<<<<< HEAD
compileExp (T.Assignment id exp) = do
    compileExpression exp
    (AddressScheme addr from) <- valueOf (symbolTable.symbol(Sym.Identifier id Nothing))
    emit [P.Store (fromIntegral addr) from]
compileExp (T.If cond thenblock elseblock) = do
    compileComparison cond
    startAddress <- valueOf currentAddress
    emit [P.Jump (Left 0)]
    compileStatements thenblock
    exitAddress <- valueOf currentAddress
    emit [P.Jump (Left 0)]
    midAddress <- valueOf currentAddress
    maybe (return ()) compileStatements elseblock
    endAddress <- valueOf currentAddress
    case cond of
        (T.LessThan _ _) -> backpatch startAddress (P.Jlt (Left midAddress))
        (T.Equal _ _) -> backpatch startAddress (P.Jeq (Left midAddress))
    backpatch exitAddress (P.Jump (Left endAddress))
compileExp (T.While cond loopbody) = do
    loopBody <- valueOf currentAddress
    compileComparison cond
    initJump <- valueOf currentAddress
    emit [P.Jump (Left 0)]
    loopSkip <- valueOf currentAddress
    emit [P.Jump (Left 0)]
    loopMain <- valueOf currentAddress
    compileStatements loopbody
    emit [P.Jump (Left loopBody)]
    endLoop <- valueOf currentAddress
    case cond of
        (T.LessThan _ _) -> backpatch initJump (P.Jlt (Left loopMain))
        (T.Equal _ _) -> backpatch initJump (P.Jeq (Left loopMain))
    backpatch loopSkip (P.Jump $ Left endLoop)
compileExp (T.ExpFunctionCall id args) = do
    emit [P.Loadi 0]
    forM_ args compileExpression
    (AddressScheme addr P.PC) <- valueOf (symbolTable.symbol(Sym.Identifier id (Just (length args))))
    emit [P.Jsr (Left addr)]
    emit [P.Pop (fromIntegral $ length args)]
    
compileComparison (T.Equal e1 e2) = do
    compileExpression e1
    compileExpression e2
    emit [P.Sub, P.Test, P.Pop 1]
compileComparison (T.LessThan e1 e2) = do
    compileExpression e1
    compileExpression e2
    emit [P.Sub, P.Test, P.Pop 1]
=======
    currentAddress .+=1
compileExp (T.Return e) = do
    -- TODO: Throw error if we attempt to return from main.
    compileExpression e
    returnAddr <- -- Need some way of getting this.
    emit [Store returnAddr FP, Rts]
    currentAddress .+= 2
compileExp (T.Assignment id e) = do
    -- TODO: Throw error on undeclared id
    compileExpression e
    (AddressScheme addr from) <- valueOf (symbolTable.symbol(Sym.Identifier x Nothing))
    emit [Store (fromIntegral addr) from]
    currentAddress .+= 1
>>>>>>> 2b6f131964caf249c5b9b442ebdd1966fb224b28

compileExpression (T.Plus e1 e2) = do
    compileExpression e1
    compileExpression e2
    emit [P.Add]
compileExpression (T.Minus e1 e2) = do
    compileExpression e1
    compileExpression e2
    emit [P.Sub]
compileExpression (T.Mult e1 e2) = do
    compileExpression e1
    compileExpression e2
    emit [P.Mul]
compileExpression (T.Literal i) = do
    emit [P.Loadi (fromIntegral i)]
<<<<<<< HEAD
compileExpression (T.Identifier id) = do
    (AddressScheme addr from) <- valueOf (symbolTable.symbol(Sym.Identifier id Nothing))
=======
    currentAddress .+= 2
compileExpression (T.Identifier id) =  do
    -- TODO: Throw error on undeclared id
    (AddressScheme addr from) <- valueOf (symbolTable.symbol(Sym.Identifier x Nothing))
>>>>>>> 2b6f131964caf249c5b9b442ebdd1966fb224b28
    emit [P.Load (fromIntegral addr) from]
compileExpression (T.FunctionCall id args) = do
    emit [P.Loadi 0]
    forM_ args compileExpression
    (AddressScheme addr P.PC) <- valueOf (symbolTable.symbol(Sym.Identifier id (Just (length args))))
    emit [P.Jsr (Left addr), P.Pop (fromIntegral $ length args)]
