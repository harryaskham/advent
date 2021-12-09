module Day9 (part1, part2) where

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
  let g = readGrid $(input 9)
   in sum (risk <$> points (minima g) g)

basin :: Grid Cell -> Coord2 -> Set Coord2
basin g p' = go (singleton p') S.empty
  where
    go Empty b = b
    go (p :<| q) b
      | p `S.member` b || g M.! p == Cell 9 = go q b
      | otherwise =
        go
          (q >< SQ.fromList [n | n <- neighborsNoDiags p, M.lookup n g >= M.lookup p g])
          (S.insert p b)

part2 :: Int
part2 =
  let g = readGrid $(input 9)
   in product . take 3 . sortOn Down $ S.size . basin g <$> minima g
