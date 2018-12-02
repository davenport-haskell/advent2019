module DayOne.ProblemOne where

import DayOne.ProblemOneInput
import Control.Exception            (Exception,SomeException, throw)

import qualified Data.Set as Set

problem1 :: IO ()
problem1 = do 
  print result1
  print result2

firstDuplicate :: Ord a => [a] -> Maybe a
firstDuplicate xs = dup' xs Set.empty
  where dup' [] _ = Nothing
        dup' (x:xs) s = if Set.member x s 
                           then Just x
                           else dup' xs (Set.insert x s)

linesString = lines input
changes = fmap parseFrequencyChange linesString

result1 = foldl toNewFrequency (Frequency 0) changes
result2 = firstDuplicate $ scanl toNewFrequency (Frequency 0) (cycle changes)

toNewFrequency :: Frequency -> FrequencyChange -> Frequency
toNewFrequency (Frequency i) (FrequencyChange Plus (Frequency change)) = Frequency $ i + change
toNewFrequency (Frequency i) (FrequencyChange Minus (Frequency change)) = Frequency $ i - change

data Signed = Plus | Minus
  deriving (Show, Eq, Ord)

data ParseError = ParseError
  deriving (Show, Eq, Ord)
instance Exception ParseError

newtype Frequency = Frequency Int
  deriving (Show, Eq, Ord)

data FrequencyChange = FrequencyChange Signed Frequency
  deriving (Show, Eq, Ord) 

parseFrequencyChange :: String -> FrequencyChange
parseFrequencyChange ('+': xs) = FrequencyChange Plus $ Frequency (read xs)
parseFrequencyChange ('-': xs) = FrequencyChange Minus $ Frequency (read xs)
parseFrequencyChange _ = throw ParseError