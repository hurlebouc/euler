module Main where

import Lib
import Data.List
import Data.Maybe

main :: IO ()
main = do
    print "coucou"

mods :: Int -> [Int] 
mods d = iterate (\r -> mod (r*10) d) 1

history' :: [a] -> [([a], [a])]
history' l = iterate (\couple -> let ((now:futur), past) = couple in (futur, now:past)) (l, [])

history :: [a] -> [[a]]
history l = (fmap snd $ history' l)

modsWitHist :: Int -> [(Int, [Int])]
modsWitHist d = let l = mods d in zip l $ history l

cycleLen :: Eq a => (a, [a]) -> Maybe Int
cycleLen (a, l) = elemIndex a l

cycleLenBool :: Eq a => (a, [a]) -> Bool
cycleLenBool c = case cycleLen c of
    Just _ -> True
    Nothing -> False

getCycleLen :: Int -> Int
getCycleLen = succ . fromJust . cycleLen . head . (dropWhile (not . cycleLenBool)) . modsWitHist