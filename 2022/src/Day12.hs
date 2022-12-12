module Day12 (part1, part2) where

import Data.Bimap qualified as BM
import Data.Map.Strict qualified as M
import Data.Sequence qualified as SQ
import Data.Set qualified as S
import Helper.Coord (neighborsNoDiags)
import Helper.Grid (Grid, GridCell (charMap), findOne, readGrid)
import Helper.TH (input)

data Cell = Start {_c :: Char} | End {_c :: Char} | Cell {_c :: Char} deriving (Eq, Ord)

instance GridCell Cell where
  charMap = BM.fromList $ [(Start 'a', 'S'), (End 'z', 'E')] ++ [(Cell c, c) | c <- ['a' .. 'z']]

floodUntil :: Grid Cell -> (Cell -> Bool) -> Maybe Int
floodUntil g p = go (SQ.singleton (findOne (End 'z') g, 0)) S.empty
  where
    go SQ.Empty _ = Nothing
    go ((currentPos, steps) SQ.:<| queue) seen
      | currentPos `S.member` seen = go queue seen
      | p (g M.! currentPos) = Just steps
      | otherwise = go (queue SQ.>< SQ.fromList ns) (S.insert currentPos seen)
      where
        ns =
          [ (n, steps + 1)
            | n <- neighborsNoDiags currentPos,
              (ord . _c <$> M.lookup n g) >= (subtract 1 . ord . _c <$> M.lookup currentPos g)
          ]

part1 :: Maybe Int
part1 = floodUntil (readGrid $(input 12)) (== Start 'a')

part2 :: Maybe Int
part2 = floodUntil (readGrid $(input 12)) (== Cell 'a')
