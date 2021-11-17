module TwentySeventeen.Day1 where

import Data.Char (digitToInt)

inputPath :: String
inputPath = "input/2017/1.txt"

sumMatching :: Int -> [Int] -> Int -> Int
sumMatching first [x] total = if x == first then total + x else total
sumMatching first (x : y : xs) total = sumMatching first (y : xs) total'
  where
    total' = if x == y then total + x else total

part1 :: IO Int
part1 = do
  xs <- fmap digitToInt . head . lines <$> readFile inputPath
  return $ sumMatching (head xs) xs 0

part2 :: IO Int
part2 = do
  xs <- fmap digitToInt . head . lines <$> readFile inputPath
  let pairs = zip xs (drop (length xs `div` 2) (cycle xs))
  return $ sum [a | (a, b) <- pairs, a == b]
