module TwentySixteen.Day13 where

import Coord (Coord2, neighborsNoDiags)
import Data.Bits (Bits (popCount))
import Data.Sequence (Seq)
import qualified Data.Sequence as SQ
import Data.Set (Set)
import qualified Data.Set as S

favourite :: Int
favourite = 1352

open :: Coord2 -> Bool
open (x, y) =
  even . popCount $
    (x * x) + (3 * x) + (2 * x * y) + y + (y * y) + favourite

getNext :: Coord2 -> Int -> Set Coord2 -> [(Coord2, Int)]
getNext pos steps visited =
  [ (p, steps + 1)
    | p@(x, y) <- neighborsNoDiags pos,
      open p,
      x >= 0,
      y >= 0,
      not (p `S.member` visited)
  ]

distanceToTarget :: Seq (Coord2, Int) -> Set Coord2 -> Coord2 -> Maybe Int
distanceToTarget SQ.Empty _ _ = Nothing
distanceToTarget ((pos, steps) SQ.:<| rest) visited target
  | pos == target = Just steps
  | otherwise =
    distanceToTarget
      (rest SQ.>< SQ.fromList (getNext pos steps visited))
      (S.insert pos visited)
      target

part1 :: Maybe Int
part1 = distanceToTarget (SQ.singleton ((1, 1), 0)) S.empty (31, 39)

locationsSeenIn :: Seq (Coord2, Int) -> Set Coord2 -> Int -> Int
locationsSeenIn SQ.Empty visited _ = S.size visited
locationsSeenIn ((pos, steps) SQ.:<| rest) visited maxSteps
  | steps > maxSteps = S.size visited
  | otherwise =
    locationsSeenIn
      (rest SQ.>< SQ.fromList (getNext pos steps visited))
      (S.insert pos visited)
      maxSteps

part2 :: Int
part2 = locationsSeenIn (SQ.singleton ((1, 1), 0)) S.empty 50
