module Day15 (part1, part2) where

import Data.List (foldl1)
import Data.PQueue.Prio.Min qualified as PQ
import Data.Set qualified as S
import Helper.Coord (Coord2, manhattan, neighbors)
import Helper.TH (input)
import Helper.Util (count, numberLine4, parseWith)
import Text.ParserCombinators.Parsec (eof, many1)

rangesCovered :: Int -> (Int, Int, Int, Int) -> Maybe (Set Coord2)
rangesCovered row (sx, sy, bx, by)
  | abs (sy - row) > d = Nothing
  | otherwise = Just (S.delete (bx, by) (S.fromList ((,row) <$> [sx - coverage .. sx + coverage])))
  where
    d = manhattan (sx, sy) (bx, by)
    coverage = d - abs (sy - row)

search :: [(Int, Int, Int, Int)] -> Int
search sensors = go (PQ.singleton (h start) start) S.empty
  where
    start = (2000000, 2000000)
    h (x, y) = count (`sees` (x, y)) sensors
    sees (sx, sy, bx, by) (x, y) = manhattan (sx, sy) (x, y) <= manhattan (sx, sy) (bx, by)
    go queue seen
      | score == 0 = x * 4000000 + y
      | (x, y) `S.member` seen = go rest seen
      | otherwise = go queue' (S.insert (x, y) seen)
      where
        ((score, (x, y)), rest) = PQ.deleteFindMin queue
        queue' =
          foldl'
            (\q n -> PQ.insert (h n) n q)
            rest
            ([n | n@(nx, ny) <- neighbors (x, y), nx >= 0, ny >= 0, nx <= 4000000, ny <= 4000000])

part1 :: Int
part1 =
  $(input 15)
    & parseWith (many1 numberLine4 <* eof)
    & mapMaybe (rangesCovered 2000000)
    & foldl1 S.union
    & S.size

part2 :: Int
part2 =
  $(input 15)
    & parseWith (many1 numberLine4 <* eof)
    & search
