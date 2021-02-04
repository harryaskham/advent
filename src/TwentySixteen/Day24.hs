module TwentySixteen.Day24 where

import Control.Monad
import Control.Monad.Memo
import Coord
import Data.Bits
import Data.Char
import qualified Data.Foldable as F
import Data.Function
import Data.List
import Data.List.Extra
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Sequence (Seq)
import qualified Data.Sequence as SQ
import Data.Set (Set)
import qualified Data.Set as S
import Data.Tuple.Extra
import Data.Vector (Vector)
import qualified Data.Vector as V
import Debug.Trace
import Grid
import Text.ParserCombinators.Parsec
import Util

data Cell = Wall | Empty (Maybe Int) deriving (Eq, Ord)

fromChar :: Char -> Cell
fromChar '#' = Wall
fromChar '.' = Empty Nothing
fromChar x = Empty (Just $ digitToInt x)

-- All pairs shortest paths between points of interest
-- including the starting point, even though we never need to go back there
-- Then it's dijkstra

onlyNumbers :: Grid Cell -> Grid Cell
onlyNumbers = M.filter f
  where
    f (Empty (Just _)) = True
    f _ = False

getNumber :: Cell -> Maybe Int
getNumber (Empty (Just x)) = Just x
getNumber _ = Nothing

shortestDistancesFrom :: Grid Cell -> Coord2 -> Map Int Int
shortestDistancesFrom grid start =
  go (SQ.singleton start) S.empty M.empty
  where
    numNumbers = M.size (onlyNumbers grid)
    go SQ.Empty _ distances = distances
    go (pos SQ.:<| queue) seen distances
      | M.size distances == numNumbers = distances
      | pos `S.member` seen = go queue seen distances
      | otherwise =
        case getNumber (grid M.! pos) of
          Just x ->
            if x `M.member` distances
              then go nextQueue nextSeen distances
              else go nextQueue nextSeen (M.insert x (S.size seen) distances)
          Nothing -> go nextQueue nextSeen distances
      where
        nextSeen = S.insert pos seen
        nextStates =
          [ n
            | n <- neighborsNoDiags pos,
              M.lookup n grid /= Just Wall
          ]
        nextQueue = queue SQ.>< SQ.fromList nextStates

shortestVisitationFrom :: Map Int (Map Int Int) -> Int -> Int
shortestVisitationFrom graph start =
  go start (S.singleton start) 0
  where
    go current visited steps
      | null remaining = traceShow visited $ steps
      | otherwise = traceShow visited $ minimum nextSteps
      where
        distances = graph M.! current
        remaining = M.filterWithKey (\k _ -> not (k `S.member` visited)) distances
        nextSteps = (\(i, d) -> go i (S.insert i visited) (steps + d)) <$> M.toList remaining

part1 :: IO Int
part1 = do
  grid <- toGrid fromChar . lines <$> input 2016 24
  let numCellToPositions = swapMap (onlyNumbers grid)
      numCellToDistances = shortestDistancesFrom grid <$> numCellToPositions
      numToDistances = M.mapKeys (unjust . getNumber) numCellToDistances
  return $ shortestVisitationFrom numToDistances 0

-- 5717 too high
