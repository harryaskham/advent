module TwentyEighteen.Day18 where

import Coord (Coord2, neighbors)
import qualified Data.Map.Strict as M
import Grid (Grid, toGrid)
import Util (input)

data Cell = Open | Wood | Lumber deriving (Eq, Ord)

fromChar :: Char -> Cell
fromChar '.' = Open
fromChar '|' = Wood
fromChar '#' = Lumber

step :: Grid Cell -> Grid Cell
step grid = M.mapWithKey (stepCell grid) grid

stepCell :: Grid Cell -> Coord2 -> Cell -> Cell
stepCell grid pos cellType =
  case cellType of
    Open -> if woodNs >= 3 then Wood else Open
    Wood -> if lumberNs >= 3 then Lumber else Wood
    Lumber -> if lumberNs >= 1 && woodNs >= 1 then Lumber else Open
  where
    ns = filter (`M.member` grid) (neighbors pos)
    woodNs = length $ filter (\n -> grid M.! n == Wood) ns
    lumberNs = length $ filter (\n -> grid M.! n == Lumber) ns

solve :: Grid Cell -> Int
solve endGrid = numWood * numLumber
  where
    numWood = M.size $ M.filter (== Wood) endGrid
    numLumber = M.size $ M.filter (== Lumber) endGrid

part1 :: IO Int
part1 = do
  grid <- toGrid fromChar . lines <$> input 2018 18
  return . solve $ iterate step grid !! 10

stepUntilCycle :: Grid Cell -> ([Grid Cell], [Grid Cell])
stepUntilCycle startGrid = go startGrid M.empty [] 0
  where
    go currentGrid seen grids n
      | currentGrid `M.member` seen = splitAt (seen M.! currentGrid) (reverse grids)
      | otherwise = go (step currentGrid) (M.insert currentGrid n seen) (currentGrid : grids) (n + 1)

part2 :: IO Int
part2 = do
  grid <- toGrid fromChar . lines <$> input 2018 18
  let (inits, cyc) = stepUntilCycle grid
  return . solve $ (inits ++ cycle cyc) !! 1000000000
