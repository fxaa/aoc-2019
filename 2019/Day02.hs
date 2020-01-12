module Day02
    ( 
    answer02a,
    answer02b,
    addStep,
    multiplyStep,
    (->>),
    (>>-)
    ) where

import Data.Vector hiding (map) 
import qualified Data.Vector

type ProgramState = (Vector Int, Int)
type Operation = [(Int, Int)]
type OpCode = Int

-- Generates the modification for multiplication in the form of [(dst, val)] where dst is the destination register and 
-- val is the sum of the two input registers. 
addStep :: ProgramState -> Operation
addStep prog = [(
    fst prog! (snd prog + 3), 
    fst prog! (fst prog! (snd prog + 1)) + fst prog! (fst prog! (snd prog + 2)) 
    )]

-- Generates the modification for multiplication in the form of [(dst, val)] where dst is the destination register and 
-- val is the product of the two input registers. 
multiplyStep :: ProgramState -> Operation
multiplyStep prog = [(
    fst prog! (snd prog + 3), 
    fst prog! (fst prog! (snd prog + 1)) * fst prog! (fst prog! (snd prog + 2)) 
    )]

-- Modifies the state of the program and returns the modified program with the next OpCode selected.
(->>) :: ProgramState -> (ProgramState -> Operation) -> ProgramState
prog ->> stepper = ((fst prog) // (stepper prog), (snd prog) + 4)

-- Returns whether the current state is at the provided OpCode.
(>>-) :: ProgramState -> OpCode -> Bool
prog >>- op = (uncurry (!) prog) == op

-- Recursively apply steps to the program until the current register does not contain a valid OpCode
continue :: ProgramState -> ProgramState
continue progState
    | progState >>- 1  = continue (progState ->> addStep)
    | progState >>- 2  = continue (progState ->> multiplyStep)
    | otherwise = progState

-- Start the computation at register 0.
compute :: Vector Int -> [(Int, Int)] -> Vector Int
compute prog [] = fst (continue (prog, 0))
compute prog replacements = fst (continue (prog // replacements, 0))

answer02a :: [Int] -> [(Int, Int)] -> [Int]
answer02a ns [] = toList $ compute ( fromList ns ) []
answer02a ns replacements = toList $ compute ( fromList ns ) replacements

answer02b :: [Int] -> [Int]
answer02b = id
