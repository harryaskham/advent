module Day9 (part1, part2) where

import Data.Array qualified as A
import Data.Bimap (Bimap)
import Data.Bimap qualified as BM
import Data.List (foldl1, foldr1)
import Data.Map.Strict qualified as M
import Data.Mod
import Data.PQueue.Prio.Min qualified as PQ
import Data.Sequence qualified as SQ
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Read
import Data.Vector qualified as V
import Helper.Coord
import Helper.Grid
import Helper.TH
import Helper.Tracers
import Helper.Util
import Relude.Unsafe qualified as U
import Text.ParserCombinators.Parsec

parser :: Parser [[Int]]
parser = many1 ((number `sepBy` string " ") <* eol) <* eof

difference :: [Int] -> [Int]
difference [] = []
difference [_] = []
difference (x : x' : xs) = (x' - x) : difference (x' : xs)

differences :: [Int] -> [[Int]]
differences = go []
  where
    go ds xs
      | all (== 0) xs = ds
      | otherwise = go (xs : ds) (difference xs)

interpolate :: ([Int] -> Int) -> (Int -> Int -> Int) -> [[Int]] -> Int
interpolate get pm dss = get (U.last dss) `pm` foldl1 (flip pm) (get <$> U.init dss)

solve :: ([Int] -> Int) -> (Int -> Int -> Int) -> Int
solve get pm =
  $(input 9)
    & parseWith parser
    & fmap (interpolate get pm . differences)
    & sum

part1 :: Int
part1 = solve U.last (+)

part2 :: Int
part2 = solve U.head (-)