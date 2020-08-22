module Main where

import Lib
import Data.List

-- correct mais lent, peut sûrement être amélioré
-- en fait quand on le compile il est rapide ^^

main :: IO ()
main = do
    print $ res 999

prime :: Integral a => [a] -> [a]
prime (a:tab) = a:prime (filter (\x -> mod x a /= 0) tab)

premier :: [Int]
premier = prime [2..]

isPrime :: Int -> Bool
isPrime n = null $ filter (\p -> mod n p == 0) $ takeWhile (\p -> p*p <=abs n) premier

isPrime2 :: Int -> Bool
isPrime2 n = (head $ dropWhile (\a -> a < abs n) premier) == n

suite :: Int -> Int -> [Int]
suite a b = fmap (\n -> n*n + a*n + b) [0..]

sizePrimeSeq :: [Int] -> Int
sizePrimeSeq = length . takeWhile isPrime

sizePrimeSeqCoef :: (Int, Int) -> Int
sizePrimeSeqCoef (a, b) = sizePrimeSeq $ suite a b

boundedPrime' :: Int -> [Int]
boundedPrime' abs = takeWhile (<= abs) premier

boundedPrime :: Int -> [Int]
boundedPrime abs = (fmap (\n -> (-n)) $ boundedPrime' abs) ++ (boundedPrime' abs)

domainCoefs :: Int -> [(Int, Int)]
domainCoefs abs = [-abs..abs] >>= \a -> fmap (\b -> (a,b)) (boundedPrime abs)

res :: Int -> (Int, Int)
res abs = maximumBy (\c1 c2 -> compare (sizePrimeSeqCoef c1) (sizePrimeSeqCoef c2)) $ domainCoefs abs