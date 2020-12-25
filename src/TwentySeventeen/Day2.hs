module TwentySeventeen.Day2 where

import Data.List.Split
import Util

inputPath :: String
inputPath = "input/2017/2.txt"

readRows :: IO [[Int]]
readRows = do
  ls <- lines <$> readFile inputPath
  return $ read <$$> splitOn "\t" <$> ls

part1 :: IO Int
part1 = do
  rows <- readRows
  let rowChecksum = (-) <$> maximum <*> minimum
  return $ sum (rowChecksum <$> rows)

part2 :: IO Int
part2 = do
  rows <- readRows
  let divider row =
        head
          [ a `div` b
            | a <- row,
              b <- row,
              a /= b,
              a `mod` b == 0
          ]
  return $ sum (divider <$> rows)
