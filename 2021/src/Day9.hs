module Day9 (part1, part2) where

import Control.Arrow (app)
import Data.Bimap qualified as BM
import Data.Char (intToDigit)
import Data.Fin
import Data.Map.Strict qualified as M
import Data.Sequence (Seq (Empty, (:<|)), singleton, (><))
import Data.Sequence qualified as SQ
import Data.Set qualified as S
import Data.Type.Nat
import Helper.Coord (Coord2, neighborsNoDiags)
import Helper.Grid (Grid, GridCell (charMap), points, readGrid)
import Helper.TH (input)

newtype Cell = Cell (Fin ('S Nat9)) deriving (Eq, Ord, Bounded)

instance GridCell Cell where
  charMap = BM.fromList [(Cell (fromInteger i), intToDigit (fromIntegral i)) | i <- [0 .. 9]]

risk :: Cell -> Integer
risk (Cell h) = toInteger h + 1

localMinima :: Ord a => Grid a -> [Coord2]
localMinima g = [p | (p, c) <- M.toList g, all (c <) (points (neighborsNoDiags p) g)]

part1 :: Integer
part1 = readGrid $(input 9) & (flip points &&& localMinima) & app & fmap risk & sum

basins :: (Bounded a, Ord a) => Grid a -> [Set Coord2]
basins g = go S.empty . singleton <$> localMinima g
  where
    go b Empty = b
    go b (p :<| q)
      | p `S.member` b || g M.! p == maxBound = go b q
      | otherwise =
        go
          (S.insert p b)
          (q >< SQ.fromList [n | n <- neighborsNoDiags p, M.lookup n g >= M.lookup p g])

part2 :: Int
part2 = (readGrid $(input 9) :: Grid Cell) & basins & fmap S.size & sortOn Down & take 3 & product
