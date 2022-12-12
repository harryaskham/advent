module Day12 (part1, part2) where

import Data.Array qualified as A
import Data.Bimap (Bimap)
import Data.Bimap qualified as BM
import Data.List (minimum)
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
import Text.ParserCombinators.Parsec

data Cell = Start Char | End Char | Cell Char deriving (Eq, Ord)

instance GridCell Cell where
  charMap = BM.fromList $ [(Start 'a', 'S'), (End 'z', 'E')] ++ [(Cell c, c) | c <- ['a' .. 'z']]

height :: Cell -> Int
height (Start c) = ord c
height (End c) = ord c
height (Cell c) = ord c

floodUntil :: Grid Cell -> (Cell -> Bool) -> Maybe Int
floodUntil g p = go (SQ.singleton (findOne (End 'z') g, 0)) S.empty
  where
    go SQ.Empty _ = Nothing
    go ((currentPos, n) SQ.:<| rest) seen
      | currentPos `S.member` seen = go rest seen
      | p (g M.! currentPos) = Just n
      | otherwise = go queue' seen'
      where
        current = M.lookup currentPos g
        ns =
          [ ((x, y), n + 1)
            | (x, y) <- neighborsNoDiags currentPos,
              let c = M.lookup (x, y) g,
              (height <$> c) >= (subtract 1 . height <$> current),
              (x, y) `S.notMember` seen
          ]
        queue' = rest SQ.>< SQ.fromList ns
        seen' = S.insert currentPos seen

part1 :: Maybe Int
part1 =
  let g = $(input 12) & readGrid
   in floodUntil g (== Start 'a')

part2 :: Maybe Int
part2 =
  let g = $(input 12) & readGrid
   in floodUntil g ((== height (Cell 'a')) . height)
