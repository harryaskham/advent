module Day15 (part1, part2) where

import Data.Map.Strict qualified as M
import Data.PQueue.Prio.Min qualified as PQ
import Data.Set qualified as S
import Helper.Coord (neighborsNoDiags)
import Helper.Grid (Grid, IntCell (IntCell), extendGrid, readGrid)
import Helper.TH (input)

lowestRisk :: Int -> Grid IntCell -> Maybe Int
lowestRisk n g = go (PQ.singleton 0 (0, 0)) M.empty
  where
    extension (IntCell c) (xOff, yOff) =
      let v = c + xOff + yOff
       in if v > 9 then v - 9 else v
    (member, lookup, end) = extendGrid n extension g
    go queue lowestRiskAt
      | null queue = Nothing
      | pos == end = Just risk'
      | not (member pos) = go rest lowestRiskAt
      | otherwise =
        case M.lookup pos lowestRiskAt of
          Nothing -> go queue' lowestRiskAt'
          Just lowest ->
            if lowest <= risk
              then go rest lowestRiskAt
              else go queue' lowestRiskAt'
      where
        ((risk, pos), rest) = PQ.deleteFindMin queue
        risk' = if pos == (0, 0) then 0 else risk + lookup pos
        next = filter member (neighborsNoDiags pos)
        queue' = foldl' (flip (PQ.insert risk')) rest next
        lowestRiskAt' = M.insertWith min pos risk lowestRiskAt

part1 :: Maybe Int
part1 = lowestRisk 1 (readGrid $(input 15))

part2 :: Maybe Int
part2 = lowestRisk 5 (readGrid $(input 15))
