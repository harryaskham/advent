module Day11 (part1, part2) where

import Data.Bimap qualified as BM
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Helper.Grid (DotHash (..), Grid, find, maxXY, readGrid)
import Helper.TH (input)
import Helper.Util (range, triPairs)
import Prelude hiding (find)

distanceSum :: Int -> Grid DotHash -> Int
distanceSum sc g =
  let es cons (a, b) = S.fromList [i | i <- [0 .. a], all (== Dot) [g M.! cons i j | j <- [0 .. b]]]
      di (ai, bi) es = sum [bool 1 sc (i `S.member` es) | i <- range ai bi]
      d (ax, ay) (bx, by) = di (ax, bx) (es (,) (maxXY g)) + di (ay, by) (es (flip (,)) (swap (maxXY g))) - 2
   in sum $ uncurry d <$> triPairs (find Hash g)

part1 :: Int
part1 = $(input 11) & readGrid & distanceSum 2

part2 :: Int
part2 = $(input 11) & readGrid & distanceSum 1000000