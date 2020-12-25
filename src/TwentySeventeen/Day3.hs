module TwentySeventeen.Day3 where

import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Debug.Trace

input :: Int
input = 277678

data Dir = N | E | S | W

turn :: Dir -> Dir
turn N = W
turn E = N
turn S = E
turn W = S

move :: (Int, Int) -> Dir -> (Int, Int)
move (x, y) N = (x, y + 1)
move (x, y) E = (x + 1, y)
move (x, y) S = (x, y -1)
move (x, y) W = (x -1, y)

spiralDistance :: Int -> Dir -> Int -> (Int, Int) -> S.Set (Int, Int) -> Int
spiralDistance target dir current c@(x, y) seen
  | current == target = abs x + abs y
  | canTurn = spiralDistance target dir (current + 1) (move c dir) seen'
  | otherwise = spiralDistance target (turn dir) (current + 1) (move c (turn dir)) seen'
  where
    seen' = S.insert (x, y) seen
    canTurn = move c (turn dir) `S.member` seen

part1 :: Int
part1 = spiralDistance input E 2 (1, 0) (S.singleton (0, 0))

neighbours :: (Int, Int) -> M.Map (Int, Int) a -> [a]
neighbours (x, y) mem =
  mapMaybe
    (flip M.lookup mem)
    [ (x + xO, y + yO)
      | xO <- [-1 .. 1],
        yO <- [-1 .. 1],
        not (xO == 0 && yO == 00)
    ]

firstLarger :: Int -> Dir -> Int -> (Int, Int) -> M.Map (Int, Int) Int -> Int
firstLarger target dir current c@(x, y) mem
  | ns > target = ns
  | canTurn = firstLarger target dir (current + 1) (move c dir) mem'
  | otherwise = firstLarger target (turn dir) (current + 1) (move c (turn dir)) mem'
  where
    ns = sum $ neighbours c mem
    mem' = (M.insert (x, y) ns mem)
    canTurn = trace (show ns) $ move c (turn dir) `M.member` mem

part2 :: Int
part2 = firstLarger input E 2 (1, 0) (M.singleton (0, 0) 1)
