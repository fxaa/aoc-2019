module Day02
    ( 
    answer02a,
    answer02b,
    addStep,
    multiplyStep,
    (->>),
    (>>-)
    ) where

import Data.Vector hiding (filter, head, last, map) 
import qualified Data.Vector
import qualified Data.List as L

type ProgramState = (Vector Int, Int)
-- Describes a list of replacement operations of the form (dst, val), where register dst is given the value val.
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

-- Generate a list of Operation pairs that replace registers 1 and 2.
-- We'll try running the program with every pair in the generated set.
ranges :: Int -> [Operation]
ranges n = [[(1, x), (2, y)] | x <- [0..n], y <- [0..n]]

computationResults :: [Int] -> Int -> [Int]
computationResults ns limit = map (head . answer02a ns) (ranges limit)

matchesTarget :: Int -> [Int] -> Operation -> Bool
matchesTarget x program replacements = x == head (answer02a program replacements)

-- Start the computation at register 0.
compute :: Vector Int -> Operation -> Vector Int
compute prog [] = fst (continue (prog, 0))
compute prog replacements = fst (continue (prog // replacements, 0))

answer02a :: [Int] -> Operation -> [Int]
answer02a ns [] = toList $ compute ( fromList ns ) []
answer02a ns replacements = toList $ compute ( fromList ns ) replacements

answer02b :: [Int] -> Int -> Int
answer02b ns 0 = 100 * head (answer02a ns [])
answer02b ns target = 
    let x = L.find (matchesTarget target ns) (ranges 100)
    in case x of 
        Nothing -> 0 
        -- This Operation of form x = [(1, a), (2, b)] gave us our solution, so use a and b in the final computation.
        Just x -> 100 * snd (head x) + snd (last x)