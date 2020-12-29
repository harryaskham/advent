module TwentySeventeen.Day22 where

import Coord
  ( Coord2,
    Dir2 (DirUp),
    move,
    turn180,
    turnCCW,
    turnCW,
  )
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Grid (Grid, maxXY, toGrid)

inputPath :: String
inputPath = "input/2017/22.txt"

data Cell
  = Clean
  | Infected
  | Weakened
  | Flagged

fromChar :: Char -> Cell
fromChar '.' = Clean
fromChar '#' = Infected

data State = State (Grid Cell) Coord2 Dir2 Int

step1 :: State -> State
step1 (State grid pos dir numInfections) =
  case M.findWithDefault Clean pos grid of
    Clean ->
      State
        (M.insert pos Infected grid)
        (move (turnCCW dir) pos)
        (turnCCW dir)
        (numInfections + 1)
    Infected ->
      State
        (M.insert pos Clean grid)
        (move (turnCW dir) pos)
        (turnCW dir)
        numInfections

solve :: (State -> State) -> Int -> IO Int
solve step n = do
  grid <- toGrid fromChar . lines <$> readFile inputPath
  let (maxX, maxY) = maxXY grid
      start = (maxX `div` 2, maxY `div` 2)
      (State _ _ _ numInfections) =
        foldl'
          (\s _ -> step s)
          (State grid start DirUp 0)
          [1 .. n]
  return numInfections

part1 :: IO Int
part1 = solve step1 10000

step2 :: State -> State
step2 (State grid pos dir numInfections) =
  case M.findWithDefault Clean pos grid of
    Clean ->
      State
        (M.insert pos Weakened grid)
        (move (turnCCW dir) pos)
        (turnCCW dir)
        numInfections
    Weakened ->
      State
        (M.insert pos Infected grid)
        (move dir pos)
        dir
        (numInfections + 1)
    Infected ->
      State
        (M.insert pos Flagged grid)
        (move (turnCW dir) pos)
        (turnCW dir)
        numInfections
    Flagged ->
      State
        (M.insert pos Clean grid)
        (move (turn180 dir) pos)
        (turn180 dir)
        numInfections

part2 :: IO Int
part2 = solve step2 10000000
