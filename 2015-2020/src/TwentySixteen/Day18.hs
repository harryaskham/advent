{-# LANGUAGE BangPatterns #-}

module TwentySixteen.Day18 where

import Data.List (foldl')
import qualified Data.Map.Strict as M
import Grid (Grid, maxXY, minXY, toGrid)
import Util (input)

data Cell = Trap | Safe deriving (Eq)

instance Show Cell where
  show Trap = "^"
  show Safe = "."

fromChar :: Char -> Cell
fromChar '^' = Trap
fromChar '.' = Safe

extendGrid :: Int -> Grid Cell -> Grid Cell
extendGrid targetY grid = go maxY grid
  where
    (minX, _) = minXY grid
    (maxX, maxY) = maxXY grid
    getCell grid (l, c, r)
      | sig == [True, True, False] = Trap
      | sig == [False, True, True] = Trap
      | sig == [True, False, False] = Trap
      | sig == [False, False, True] = Trap
      | otherwise = Safe
      where
        sig = (== Just Trap) <$> (M.lookup <$> [l, c, r] <*> pure grid)
    go y !grid
      | y + 1 == targetY = grid
      | otherwise =
        go
          (y + 1)
          ( foldl'
              (\m (pos, c) -> M.insert pos c m)
              grid
              [ ((x, y + 1), getCell grid ((x -1, y), (x, y), (x + 1, y)))
                | x <- [minX .. maxX]
              ]
          )

solve :: Int -> IO Int
solve n =
  M.size
    . M.filter (== Safe)
    . extendGrid n
    . toGrid fromChar
    . lines
    <$> input 2016 18

part1 :: IO Int
part1 = solve 40

part2 :: IO Int
part2 = solve 400000
