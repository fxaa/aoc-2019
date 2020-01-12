module Main where

import Day01
import Day02

import Data.List.Split

main = do
    content <- readFile "exec/input/2019/Day01/input.txt"
    let linesOfContent = map read $ lines content
    print $ show $ answer01a linesOfContent
    print $ show $ answer01b linesOfContent
    content <- readFile "exec/input/2019/Day02/input.txt"
    let linesOfContent = map read $ splitOn "," content
    print $ show $ answer02a linesOfContent [(1, 12), (2, 2)]

