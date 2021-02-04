module TwentySixteen.Day24 where

import Coord (Coord2, neighborsNoDiags)
import Data.Char (digitToInt)
import Data.Map (Map)
import qualified Data.Map.Strict as M
import qualified Data.Sequence as SQ
import qualified Data.Set as S
import Grid (Grid, toGrid)
import Util (input, swapMap, unjust)

data Cell = Wall | Empty (Maybe Int) deriving (Ord, Eq)

fromChar :: Char -> Cell
fromChar '#' = Wall
fromChar '.' = Empty Nothing
fromChar x = Empty (Just $ digitToInt x)

onlyNumbers :: Grid Cell -> Grid Cell
onlyNumbers = M.filter f
  where
    f (Empty (Just _)) = True
    f _ = False

getNumber :: Cell -> Maybe Int
getNumber (Empty (Just x)) = Just x
getNumber _ = Nothing

shortestDistanceBetween :: Grid Cell -> Coord2 -> Coord2 -> Int
shortestDistanceBetween grid start end =
  go (SQ.singleton (start, 0)) S.empty
  where
    go ((pos, steps) SQ.:<| queue) seen
      | pos == end = steps
      | pos `S.member` seen = go queue seen
      | otherwise =
        go nextQueue (S.insert pos seen)
      where
        nextStates =
          [ (n, steps + 1)
            | n <- neighborsNoDiags pos,
              M.lookup n grid /= Just Wall
          ]
        nextQueue = queue SQ.>< SQ.fromList nextStates

shortestVisitationFrom :: Map Int (Map Int Int) -> Int -> Maybe Int -> Int
shortestVisitationFrom graph start finalDestination =
  go start (S.singleton start) 0
  where
    go current visited steps
      | null remaining = case finalDestination of
        Nothing -> steps
        Just d -> steps + distances M.! d
      | otherwise = minimum nextSteps
      where
        distances = graph M.! current
        remaining = M.filterWithKey (\k _ -> not (k `S.member` visited)) distances
        nextSteps = (\(i, d) -> go i (S.insert i visited) (steps + d)) <$> M.toList remaining

part12 :: IO (Int, Int)
part12 = do
  grid <- toGrid fromChar . lines <$> input 2016 24
  let numToPosition = M.mapKeys (unjust . getNumber) $ swapMap (onlyNumbers grid)
      numToDistances =
        M.fromList
          [ ( a,
              M.fromList
                [ ( b,
                    shortestDistanceBetween
                      grid
                      (numToPosition M.! a)
                      (numToPosition M.! b)
                  )
                  | b <- M.keys numToPosition
                ]
            )
            | a <- M.keys numToPosition
          ]
  return $
    ( shortestVisitationFrom numToDistances 0 Nothing,
      shortestVisitationFrom numToDistances 0 (Just 0)
    )
