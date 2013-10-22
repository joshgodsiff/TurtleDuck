-- | Module for representing the PDPlot machine
-- by Robert 'Probie' Offner and Joshua Godsiff

module PDPlot where

import Data.Int
import Data.Map (Map)
import qualified Data.Map as M

data TargetPointer = FP | GP

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

type Label = Int

type LookupTable = Map Int Int16

-- A machine word is either a 16-bit integer or a label to be resolved later
type MachineWord = Either Int16 Label

type Address = MachineWord

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
toMachineCode (Load  x GP) = [Left (0x0600 + (fromIntegral x))]
toMachineCode (Load  x FP) = [Left (0x0700 + (fromIntegral x))]
toMachineCode (Store x GP) = [Left (0x0400 + (fromIntegral x))]
toMachineCode (Store x FP) = [Left (0x0500 + (fromIntegral x))]
toMachineCode (Read  x GP) = [Left (0x0200 + (fromIntegral x))]
toMachineCode (Read  x FP) = [Left (0x0300 + (fromIntegral x))]
toMachineCode (Jsr   x)    = [Left 0x6800, x]
toMachineCode (Jump  x)    = [Left 0x7000, x]
toMachineCode (Jeq   x)    = [Left 0x7200, x]
toMachineCode (Jlt   x)    = [Left 0x7400, x]
toMachineCode (Loadi x)    = [Left 0x5600, Left x]
toMachineCode (Pop   x)    = [Left 0x5e00, Left x]
-- 'Move' instruction missing?

resolveLabels :: MachineWord -> LookupTable -> Int16
resolveLabels (Left x ) _ = x
resolveLabels (Right x) t = case M.lookup x t of
    Just y -> y
    Nothing -> error $ "Label " ++ (show x) ++ " not found"

assemble :: [Instruction] -> LookupTable -> [Int16]
assemble x t = map (flip resolveLabels t) $ concatMap toMachineCode x