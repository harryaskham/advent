module TwentyEighteen where

import Data.Set

freqsToNums :: IO [Int]
freqsToNums = do
  content <- readFile "input/2018/1.txt"
  return $ parseLine <$> lines content
    where
      parseLine :: String -> Int
      parseLine (sign:number) =
        case sign of
          '+' -> read number
          '-' -> -1 * read number

day1_1 :: IO Int
day1_1 = do
  nums <- freqsToNums
  return $ sum nums

day1_2 :: IO Int
day1_2 = do
  nums <- cycle <$> freqsToNums
  return $ next empty nums 0
    where
      next :: Set Int -> [Int] -> Int -> Int
      next seen (n:ns) frequency = if frequency `member` seen then frequency else next (insert frequency seen) ns (frequency + n)
