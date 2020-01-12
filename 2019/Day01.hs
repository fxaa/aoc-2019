module Day01
    ( 
    answer01a,
    answer01b
    ) where

fuelReq :: Int -> Int
fuelReq n = (div n 3) - 2

totalReq = sum . map fuelReq

fuelReqForFuel :: Int -> Int
fuelReqForFuel n
    | n <= 0 = 0
    | n >= 1 = (+) n $ fuelReqForFuel $ fuelReq n

answer01a :: [Int] -> Int
answer01a = totalReq

answer01b :: [Int] -> Int
answer01b = sum . map (fuelReqForFuel . fuelReq)
