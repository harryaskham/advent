module Day1 (part1, part2) where

import Data.Text.Read (decimal)
import Helper.Util (batch3, input, readInput, toList3)

numIncreasing :: [Int] -> Int
numIncreasing xs = length . filter ((>) <$> fst <*> snd) . zip (drop 1 xs) $ xs

part1 :: IO Int
part1 = numIncreasing <$> readInput decimal (input 1)

part2 :: IO Int
part2 = numIncreasing . fmap sum . batch3 <$> readInput decimal (input 1)
