module Day1 (part1, part2) where

import Data.Text.Read (decimal)
import Helper.TH (inputL)
import Helper.Util (batch3, readInputL)

numIncreasing :: [Int] -> Int
numIncreasing xs =
  xs
    & zip (drop 1 xs)
    & filter ((>) <$> fst <*> snd)
    & length

part1 :: Int
part1 =
  $(inputL 1)
    & readInputL decimal
    & numIncreasing

part2 :: Int
part2 =
  $(inputL 1)
    & readInputL decimal
    & batch3
    & fmap sum
    & numIncreasing
