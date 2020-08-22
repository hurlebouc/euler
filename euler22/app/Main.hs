module Main where

import Lib
import Data.List
import System.Environment
import Data.List.Split

main :: IO ()
main = do
    content <- readFile "names.txt"
    print $ compute $ splitOn "," $ filter (\c -> c /= '"') content

compute :: [String] -> Int
compute ws = 
    let values = fmap (\word -> foldl (\n c -> n + (fromEnum c) - 64) 0 word) (sort ws)
        scores = zipWith (\v i -> v * i) values [1..]
    in foldl' (+) 0 scores