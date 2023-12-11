module Day11 (part1, part2) where

import Data.Bimap qualified as BM
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Helper.Grid (DotHash (..), Grid, find, maxXY, readGrid)
import Helper.TH (input)
import Prelude hiding (find)

pairwiseDistances :: Int -> Grid DotHash -> Int
pairwiseDistances scale g =
  let empties cons (a, b) = S.fromList [i | i <- [0 .. a], all (== Dot) [g M.! cons i j | j <- [0 .. b]]]
      d (ai, bi) es = sum [o | i <- [min ai bi .. max ai bi], let o = if i `S.member` es then scale else 1]
      distance (ax, ay) (bx, by) = d (ax, bx) (empties (,) (maxXY g)) + d (ay, by) (empties (flip (,)) (swap (maxXY g))) - 2
   in sum [distance a b | (i, a) <- zip [1 ..] (find Hash g), b <- drop i (find Hash g)]

part1 :: Int
part1 = $(input 11) & readGrid & pairwiseDistances 2

part2 :: Int
part2 = $(input 11) & readGrid & pairwiseDistances 1000000