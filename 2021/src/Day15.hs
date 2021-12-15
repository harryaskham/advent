module Day15 (part1, part2) where

import Data.Map.Strict qualified as M
import Data.PQueue.Prio.Min qualified as PQ
import Data.Set qualified as S
import Helper.Coord (neighborsNoDiags)
import Helper.Grid (DigitCell, Grid, cellToInt, incrementMod9, maxXY, readGrid)
import Helper.TH (input)
import Helper.Util (both)

lowestRisk :: Grid DigitCell -> Int -> (Int, Int) -> Maybe Int
lowestRisk g n end = go (PQ.singleton 0 (0, 0)) M.empty
  where
    (member, lookup) = extendGrid g n
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

extendGrid :: Grid DigitCell -> Int -> ((Int, Int) -> Bool, (Int, Int) -> Int)
extendGrid g n = (member, lookup)
  where
    (w, h) = both (+ 1) (maxXY g)
    lookup (x, y) =
      let (xMod, x') = x `divMod` w
          (yMod, y') = y `divMod` h
       in fromInteger $ cellToInt (incrementMod9 (xMod + yMod) (g M.! (x', y')))
    member (x, y) = x >= 0 && y >= 0 && x < w * n && y < h * n

part1 :: Maybe Int
part1 =
  let g = readGrid $(input 15)
   in lowestRisk g 1 (99, 99)

part2 :: Maybe Int
part2 =
  let g = readGrid $(input 15)
   in lowestRisk g 5 (499, 499)
