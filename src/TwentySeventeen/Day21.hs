{-# LANGUAGE TupleSections #-}

module TwentySeventeen.Day21 where

import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Grid
  ( Grid,
    joinGrids,
    maxXY,
    splitGrid,
    toGrid,
    variants,
  )

inputPath :: String
inputPath = "input/2017/21.txt"

data Cell = On | Off deriving (Show, Eq, Ord)

fromChar :: Char -> Cell
fromChar '.' = Off
fromChar '#' = On

parseRule :: String -> [(Grid Cell, Grid Cell)]
parseRule rule = (,rhs) <$> variants lhs
  where
    [lhs, rhs] = toGrid fromChar <$> (splitOn "/" <$> splitOn " => " rule)

type RuleMap = M.Map (Grid Cell) (Grid Cell)

readRules :: IO RuleMap
readRules = do
  ls <- lines <$> readFile inputPath
  return . M.fromList $ parseRule =<< ls

step :: RuleMap -> Grid Cell -> Grid Cell
step rules grid = joinGrids expanded
  where
    (maxX, _) = maxXY grid
    stride = if (maxX + 1) `mod` 2 == 0 then 2 else 3
    sGrid = splitGrid stride stride grid
    expanded = (rules M.!) <$> sGrid

start :: Grid Cell
start =
  toGrid fromChar $
    [ ".#.",
      "..#",
      "###"
    ]

part12 :: IO [Int]
part12 = do
  rules <- readRules
  let states = (iterate (step rules) start !!) <$> [5, 18]
  return $ M.size . M.filter (== On) <$> states
