-- | Module for representing the PDPlot machine
-- by Robert 'Probie' Offner and Joshua Godsiff

module PDPlot where

import Data.Int
import Data.Word
import Data.Map (Map)
import qualified Data.Map as M
import SymbolTable (Identifier)
import Data.Bits

data TargetPointer = FP | GP | PC
    deriving Show

data Instruction = Load Int8 TargetPointer
                 | Loadi Int16
                 | Store Int8 TargetPointer
                 | Pop Int16
                 | Add
                 | Sub
                 | Neg
                 | Mul
                 | Test
                 | Up
                 | Down
                 | Move
                 | Read Int8 TargetPointer
                 | Jump Address
                 | Jeq Address
                 | Jlt Address
                 | Halt
                 | Jsr Address
                 | Rts
    deriving Show

-- Not sure how this was supposed to work as an Int.
type Label = Identifier

type LookupTable = Map Label Int16

-- A machine word is either a 16-bit integer or a label to be resolved later
type MachineWord = Either Int16 Label

type Address = MachineWord

instructionLength :: Instruction -> Int16
instructionLength (Pop _) = 2
instructionLength (Loadi _) = 2
instructionLength (Jump _) = 2
instructionLength (Jsr _) = 2
instructionLength (Jeq _) = 2
instructionLength (Jlt _) = 2
instructionLength _ = 1


toMachineCode :: Instruction -> [MachineWord]
toMachineCode Halt         = [Left 0x0000]
toMachineCode Up           = [Left 0x0a00]
toMachineCode Down         = [Left 0x0c00]
toMachineCode Add          = [Left 0x1000]
toMachineCode Sub          = [Left 0x1200]
toMachineCode Neg          = [Left 0x2200]
toMachineCode Mul          = [Left 0x1400]
toMachineCode Test         = [Left 0x1600]
toMachineCode Rts          = [Left 0x2800]
toMachineCode (Load  x GP) = [Left (0x0600 .|. (fromIntegral (fromIntegral x :: Word8) :: Int16))]
toMachineCode (Load  x FP) = [Left (0x0700 .|. (fromIntegral (fromIntegral x :: Word8) :: Int16))]
toMachineCode (Store x GP) = [Left (0x0400 .|. (fromIntegral (fromIntegral x :: Word8) :: Int16))]
toMachineCode (Store x FP) = [Left (0x0500 .|. (fromIntegral (fromIntegral x :: Word8) :: Int16))]
toMachineCode (Read  x GP) = [Left (0x0200 .|. (fromIntegral (fromIntegral x :: Word8) :: Int16))]
toMachineCode (Read  x FP) = [Left (0x0300 .|. (fromIntegral (fromIntegral x :: Word8) :: Int16))]
toMachineCode (Jsr   x)    = [Left 0x6800, x]
toMachineCode (Jump  x)    = [Left 0x7000, x]
toMachineCode (Jeq   x)    = [Left 0x7200, x]
toMachineCode (Jlt   x)    = [Left 0x7400, x]
toMachineCode (Loadi x)    = [Left 0x5600, Left x]
toMachineCode (Pop   x)    = [Left 0x5e00, Left x]
toMachineCode Move         = [Left 0x0e00]

resolveLabels :: MachineWord -> LookupTable -> Int16
resolveLabels (Left x ) _ = x
resolveLabels (Right x) t = case M.lookup x t of
    Just y -> y
    Nothing -> error $ "Label " ++ (show x) ++ " not found"

assemble :: [Instruction] -> [Int16]
assemble x = map (\(Left x) -> x) $ concatMap toMachineCode x
