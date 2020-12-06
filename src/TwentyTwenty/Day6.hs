module TwentyTwenty.Day6 where

import Data.List (nub)
import Data.List.Split (splitOn)
import qualified Data.Set as S

inputPath :: String
inputPath = "input/2020/6.txt"

groups :: [String] -> [[String]]
groups lines = splitOn [""] lines

part1 :: IO Int
part1 = do
  gs <- groups . lines <$> readFile inputPath
  return . sum $ length . nub . concat <$> gs

part2 :: IO Int
part2 = do
  gs <- groups . lines <$> readFile inputPath
  return . sum $ S.size . foldl1 S.intersection . fmap S.fromList <$> gs
