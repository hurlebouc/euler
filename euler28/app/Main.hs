module Main where

import Lib
import GHC.Float

main :: IO ()
main = print $ sumDiag $ 1001*1001

r :: Int -> Int
r n = 1 + (floor $ ((sqrt $ int2Float $ n - 1) - 1)/2)

f :: Int -> Int
f n = mod n (2*(r n))

diag :: Int -> [Int]
diag len = 1 : filter (\n -> f n == 1) [2..len]

sumDiag :: Int -> Int
sumDiag len = foldl (+) 0 $ diag len