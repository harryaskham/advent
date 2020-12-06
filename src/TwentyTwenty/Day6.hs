module TwentyTwenty.Day6 where

import Data.List (nub)
import Data.List.Split (splitOn)
import qualified Data.Set as S

inputPath :: String
inputPath = "input/2020/6.txt"

part1 :: IO Int
part1 = do
  groups <- splitOn [""] . lines <$> readFile inputPath
  return . sum $ length . nub . concat <$> groups

part2 :: IO Int
part2 = do
  groups <- splitOn [""] . lines <$> readFile inputPath
  return . sum $ S.size . foldl1 S.intersection . fmap S.fromList <$> groups
