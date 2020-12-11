module TwentyTwenty.Day11 where

import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)

inputPath :: String
inputPath = "input/2020/11.txt"

width :: Int
width = 98

height :: Int
height = 97

data Cell = Floor | Empty | Full deriving (Eq)

fromChar :: Char -> Cell
fromChar '.' = Floor
fromChar 'L' = Empty

type Grid = M.Map (Int, Int) Cell

fromLines :: [String] -> Grid
fromLines ls =
  M.fromList
    [ ((x, y), fromChar c)
      | (y, row) <- zip [0 ..] ls,
        (x, c) <- zip [0 ..] row
    ]

type AdjacencyFn = Int -> Int -> Grid -> [Cell]

adjacents :: AdjacencyFn
adjacents x y grid =
  catMaybes $
    M.lookup
      <$> [ (x', y')
            | x' <- [x -1 .. x + 1],
              y' <- [y -1 .. y + 1],
              (x, y) /= (x', y')
          ]
      <*> pure grid

stepGrid :: AdjacencyFn -> Int -> Grid -> Grid
stepGrid adj threshold grid =
  M.fromList
    [ ((x, y), newCell x y)
      | x <- [0 .. width - 1],
        y <- [0 .. height - 1]
    ]
  where
    newCell x y =
      let fullAdj = length $ filter (== Full) (adj x y grid)
       in case M.lookup (x, y) grid of
            Just Empty -> if fullAdj == 0 then Full else Empty
            Just Full -> if fullAdj >= threshold then Empty else Full
            Just Floor -> Floor

fixGrid :: AdjacencyFn -> Int -> Grid -> Grid
fixGrid adj threshold grid =
  let grid' = stepGrid adj threshold grid
   in if grid == grid' then grid else fixGrid adj threshold grid'

part1 :: IO Int
part1 =
  length
    . filter (== Full)
    . fmap snd
    . M.toList
    . fixGrid adjacents 4
    . fromLines
    . lines
    <$> readFile inputPath

firstChairsFrom :: Grid -> Int -> Int -> [(Int, Int)]
firstChairsFrom grid x y =
  catMaybes $
    firstChair
      <$> [ [(x', y) | x' <- [x + 1 .. width - 1]],
            [(x', y) | x' <- reverse [0 .. x - 1]],
            [(x, y') | y' <- [y + 1 .. height - 1]],
            [(x, y') | y' <- reverse [0 .. y - 1]],
            zip [x + 1 .. width - 1] [y + 1 .. height - 1],
            zip [x + 1 .. width - 1] (reverse [0 .. y - 1]),
            zip (reverse [0 .. x - 1]) [y + 1 .. height - 1],
            zip (reverse [0 .. x - 1]) (reverse [0 .. y - 1])
          ]
  where
    firstChair :: [(Int, Int)] -> Maybe (Int, Int)
    firstChair [] = Nothing
    firstChair (c : cs) = case M.lookup c grid of
      Just Floor -> firstChair cs
      Just _ -> Just c

firstChairMap :: Grid -> M.Map (Int, Int) [(Int, Int)]
firstChairMap grid =
  M.fromList
    [ ((x, y), firstChairsFrom grid x y)
      | x <- [0 .. width - 1],
        y <- [0 .. height - 1]
    ]

firstSeen :: M.Map (Int, Int) [(Int, Int)] -> AdjacencyFn
firstSeen chairMap x y grid =
  case M.lookup (x, y) chairMap of
    Just cs -> catMaybes $ M.lookup <$> cs <*> pure grid

part2 :: IO Int
part2 = do
  grid <- fromLines . lines <$> readFile inputPath
  let chairMap = firstChairMap grid
  return $
    length
      . filter (== Full)
      . fmap snd
      . M.toList
      . fixGrid (firstSeen chairMap) 5
      $ grid
