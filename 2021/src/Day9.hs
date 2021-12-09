module Day9 (part1, part2) where

import Control.Arrow (app)
import Data.Bimap qualified as BM
import Data.Char (intToDigit)
import Data.Map.Strict qualified as M
import Data.Sequence (Seq (Empty, (:<|)), singleton, (><))
import Data.Sequence qualified as SQ
import Data.Set qualified as S
import Helper.Coord (Coord2, neighborsNoDiags)
import Helper.Grid (Grid, GridCell (charMap), points, readGrid)
import Helper.TH (input)

newtype Cell = Cell Int deriving (Eq, Ord)

instance GridCell Cell where
  charMap = BM.fromList [(Cell i, intToDigit i) | i <- [0 .. 9]]

risk :: Cell -> Int
risk (Cell h) = h + 1

minima :: Grid Cell -> [Coord2]
minima g = [p | (p, c) <- M.toList g, all (c <) (points (neighborsNoDiags p) g)]

part1 :: Int
part1 =
  readGrid $(input 9)
    & (flip points &&& minima)
    & app
    & fmap risk
    & sum

basins :: Grid Cell -> [Set Coord2]
basins g = go S.empty . singleton <$> minima g
  where
    go b Empty = b
    go b (p :<| q)
      | p `S.member` b || g M.! p == Cell 9 = go b q
      | otherwise =
        go
          (S.insert p b)
          (q >< SQ.fromList [n | n <- neighborsNoDiags p, M.lookup n g >= M.lookup p g])

part2 :: Int
part2 =
  readGrid $(input 9)
    & basins
    & fmap S.size
    & sortOn Down
    & take 3
    & product
