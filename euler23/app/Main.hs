module Main where

import Lib

divisors :: Int -> [Int]
divisors a = [p | p <- [1..a-1], mod a p == 0]

divisorsSum :: Int -> Int
divisorsSum = sum . divisors

perfect :: Int -> Bool
perfect a = divisorsSum a == a

deficiant :: Int -> Bool
deficiant a = divisorsSum a < a

abundant :: Int -> Bool
abundant a = divisorsSum a > a

abundantList :: [Int]
abundantList = [a | a <- [1..], abundant a]

abundantCrible :: [Bool]
abundantCrible = [abundant i | i <- [1..]]

compositeList :: Int -> [(Int, Int)]
compositeList n = [(a, n - a) | a <- [1 .. div n 2],  abundantCrible !! (a-1) && abundantCrible !! (n - a - 1)]

headOption :: [a] -> Maybe a
headOption (a:l) = Just a
headOption [] = Nothing

isComposite :: Int -> Bool
isComposite n = case headOption (compositeList n) of
  Just _ -> True
  Nothing -> False

type Permutation = [Int]

permApply :: Permutation -> Int -> Int
permApply s n = s !! n

inv :: Permutation -> Permutation
inv s = let size = length s in [j | i <- [0 .. size -1], j <- [0 .. size -1], s !! i ==j]

permuteList :: Permutation -> [a] -> [a]
permuteList s l = [l !! (permApply s i) | i <- [0 .. (length s) - 1]]

subId :: Int -> Int -> [Int]
subId upperBound hole = [0 .. hole - 1] ++ [hole + 1 .. upperBound]

sigma :: Int -> [[Int]]
sigma 1 = [[0]]
sigma size = concat [map (\s -> i:(permuteList s (subId (size-1) i))) (sigma (size-1)) | i <- [0..size - 1]]

fiboNext :: (Integer, Integer) -> (Integer, Integer)
fiboNext (a, b) = (b, a+b)

logSize :: Integer -> Integer
logSize 0 = 0
logSize n = 1 + logSize (div n 10)

listFibo :: [(Integer, Integer, Integer)]
listFibo = iterate (\(i, a, b) -> let (a', b') = fiboNext (a, b) in (i+1, a', b')) (1, 1,1)

bigListFibo :: Integer -> [(Integer, Integer, Integer)]
bigListFibo size = filter (\(i, a, b) -> logSize a >= size) listFibo

main :: IO ()
main = do
--  print (sum [a | a <- [1 .. 28123], not (isComposite a)])
-- print (sigma 10 !! 999999)
  print (let (i, _, _) = head (bigListFibo 1000) in i)
