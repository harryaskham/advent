module TwentyTwenty.Day6 where

import Data.List (nub)
import Data.List.Split (splitOn)
import qualified Data.Set as S
import Util ((<$$>))

inputPath :: String
inputPath = "input/2020/6.txt"

groups :: IO [[String]]
groups = splitOn [""] . lines <$> readFile inputPath

part1 :: IO Int
part1 = sum <$> length . nub . concat <$$> groups

part2 :: IO Int
part2 = sum <$> S.size . foldl1 S.intersection . fmap S.fromList <$$> groups
