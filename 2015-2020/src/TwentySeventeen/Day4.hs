module TwentySeventeen.Day4 where

import Data.List (nub)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Monoid (Sum (Sum, getSum))

inputPath :: String
inputPath = "input/2017/4.txt"

part1 :: IO Int
part1 = do
  ls <- lines <$> readFile inputPath
  let valid l = let ws = splitOn " " l in length ws == length (nub ws)
  return $ length (filter valid ls)

letterCounts :: String -> M.Map Char Int
letterCounts w = getSum <$> M.fromListWith (<>) (zip w (repeat $ Sum 1))

part2 :: IO Int
part2 = do
  ls <- lines <$> readFile inputPath
  let valid l =
        let ws = splitOn " " l
            cs = letterCounts <$> ws
         in length cs == length (nub cs)
  return $ length (filter valid ls)
