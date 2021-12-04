module Day1 (part1, part2) where

import Data.Text.Read (decimal)
import Helper.TH (inputL)
import Helper.Util (batch3, input, readInput, readInputL, toList3)

numIncreasing :: [Int] -> Int
numIncreasing xs = length . filter ((>) <$> fst <*> snd) . zip (drop 1 xs) $ xs

part1 :: IO Int
part1 = return $ numIncreasing (readInputL decimal ($(inputL 1)))

part2 :: IO Int
part2 = return $ numIncreasing (sum <$> batch3 (readInputL decimal ($(inputL 1))))
