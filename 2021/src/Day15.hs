module Day15 (part1, part2) where

import Data.Map.Strict qualified as M
import Data.PQueue.Prio.Min qualified as PQ
import Data.Set qualified as S
import Helper.Coord (manhattan, neighborsNoDiags)
import Helper.Grid (DigitCell, Grid, cellToInt, incrementMod9, intToCell, joinGrids, maxXY, readGrid)
import Helper.TH (input)

lowestRisk :: Grid DigitCell -> Maybe Int
lowestRisk g = go (PQ.singleton 0 ((0, 0), S.empty, 0)) M.empty
  where
    end = maxXY g
    cost ((x, y), _, risk) = risk + manhattan (x, y) end
    go queue lowestRiskAt
      | null queue = Nothing
      | pos == end = Just risk'
      | pos `S.member` visited = go rest lowestRiskAt
      | otherwise =
        case M.lookup pos lowestRiskAt of
          Nothing -> go queue' lowestRiskAt'
          Just lowest ->
            if lowest <= risk
              then go rest lowestRiskAt
              else go queue' lowestRiskAt'
      where
        ((_, (pos, visited, risk)), rest) = PQ.deleteFindMin queue
        risk' = if pos == (0, 0) then 0 else risk + fromIntegral (cellToInt (g M.! pos))
        next = [(n, S.insert pos visited, risk') | n <- neighborsNoDiags pos, n `M.member` g]
        queue' = foldl' (\q n -> PQ.insert (cost n) n q) rest next
        lowestRiskAt' = M.insertWith min pos risk lowestRiskAt

part1 :: Maybe Int
part1 = lowestRisk (readGrid $(input 15))

part2 :: Maybe Int
part2 =
  let g = readGrid $(input 15)
   in lowestRisk . joinGrids . M.fromList $
        [ ((x, y), incrementMod9 (x + y) <$> g)
          | x <- [0 .. 4],
            y <- [0 .. 4]
        ]
