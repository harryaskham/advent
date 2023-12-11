module Day11 (part1, part2) where

import Data.Bimap qualified as BM
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Helper.Grid (DotHash (..), Grid, find, maxXY, readGrid)
import Helper.TH (input)
import Helper.Util (range)
import Prelude hiding (find)

distanceSum :: Int -> Grid DotHash -> Int
distanceSum sc g =
  let es cons (a, b) = S.fromList [i | i <- [0 .. a], all (== Dot) [g M.! cons i j | j <- [0 .. b]]]
      d (ai, bi) es = sum [o | i <- range ai bi, let o = bool sc 1 (i `S.member` es)]
      distance (ax, ay) (bx, by) = d (ax, bx) (es (,) (maxXY g)) + d (ay, by) (es (flip (,)) (swap (maxXY g))) - 2
   in sum [distance a b | (i, a) <- zip [1 ..] (find Hash g), b <- drop i (find Hash g)]

part1 :: Int
part1 = $(input 11) & readGrid & distanceSum 2

part2 :: Int
part2 = $(input 11) & readGrid & distanceSum 1000000