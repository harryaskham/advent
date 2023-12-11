module Day11 (part1, part2) where

import Data.Bimap qualified as BM
import Data.Foldable.Unicode ((∈))
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Helper.Grid (DotHash (..), Grid, gridFind, maxXY, readGrid)
import Helper.TH (input)
import Helper.Util (both, fsts2, range, snds2, triPairs)

distanceSum :: Int -> Grid DotHash -> Int
distanceSum sc g =
  let es cons (a, b) = S.fromList [i | i <- [0 .. a], all (== Dot) [g M.! cons i j | j <- [0 .. b]]]
      di (a, b) e = sum [bool 1 sc (i ∈ e) | i <- range a b]
      d ab = let m = maxXY g in di (fsts2 ab) (es (,) m) + di (snds2 ab) (es (flip (,)) (swap m)) - 2
   in sum $ d <$> triPairs (gridFind Hash g)

part1 :: Int
part1 = $(input 11) & readGrid & distanceSum 2

part2 :: Int
part2 = $(input 11) & readGrid & distanceSum 1000000