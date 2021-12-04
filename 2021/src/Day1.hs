module Day1 (part1, part2) where

import Data.Text.Read (decimal)
import Helper.TH (inputL)
import Helper.Util (batch3, readInputL)

numIncreasing :: [Int] -> Int
numIncreasing xs = length . filter ((>) <$> fst <*> snd) . zip (drop 1 xs) $ xs

part1 :: Int
part1 =
  readInputL decimal $(inputL 1)
    & numIncreasing

part2 :: Int
part2 =
  readInputL decimal $(inputL 1)
    & batch3
    & fmap sum
    & numIncreasing
