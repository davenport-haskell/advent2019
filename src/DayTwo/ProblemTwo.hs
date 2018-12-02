{-# LANGUAGE ScopedTypeVariables #-}
module DayTwo.ProblemTwo where

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.List    (elemIndices)
import DayTwo.ProblemTwoInput

linesString = lines input

problem1 = print $ (sum two) * (sum three)
  where 
    two = fmap (\s -> countHowManyValues (countN s) 2) linesString
    three = fmap (\s -> countHowManyValues (countN s) 3) linesString



countHowManyValues :: Eq b =>  Map.Map a b -> b -> Int
countHowManyValues m i = out
  where
    ele = Map.elems m
    int = count i ele
    out = if int >=1
            then 1 
            else 0


countN:: Ord a => [a] -> Map.Map a Int
countN xs =  Map.fromList output
  where
    uniqueA = unique xs
    output = fmap (\a -> (a, count a xs)) uniqueA

count :: Eq a => a -> [a] -> Int
count a xs = length $ elemIndices a xs


unique :: Ord a => [a] -> [a]
unique xs = Set.toList $ dup' xs Set.empty
  where dup' [] s = s
        dup' (x:xs) s = if Set.member x s 
                          then dup' xs s
                          else dup' xs (Set.insert x s)

