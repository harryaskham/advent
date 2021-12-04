module Day1 (part1, part2) where

import Data.Text.Read (decimal)
import Helper.TH (input)
import Helper.Util (batch3, readAs)

numIncreasing :: [Int] -> Int
numIncreasing xs =
  xs
    & zip (drop 1 xs)
    & filter ((>) <$> fst <*> snd)
    & length

part1 :: Int
part1 =
  $(input 1)
    & readAs decimal
    & numIncreasing

part2 :: Int
part2 =
  $(input 1)
    & readAs decimal
    & batch3
    & fmap sum
    & numIncreasing
