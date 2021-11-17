module TwentySeventeen.Day11 where

import Data.List (foldl')
import Data.List.Split (splitOn)

inputPath :: String
inputPath = "input/2017/11.txt"

move :: (Int, Int) -> String -> (Int, Int)
move (x, y) dir =
  case dir of
    "n" -> (x, y + 2)
    "s" -> (x, y - 2)
    "ne" -> (x + 1, y + 1)
    "nw" -> (x - 1, y + 1)
    "se" -> (x + 1, y - 1)
    "sw" -> (x - 1, y - 1)

distance :: (Int, Int) -> Int
distance (x, y)
  | y > 0 = x + ((y - abs x) `div` 2)
  | otherwise = x + ((y + abs x) `div` 2)

part1 :: IO Int
part1 = do
  dirs <- splitOn "," . head . lines <$> readFile inputPath
  return . distance $ foldl' move (0, 0) dirs

part2 :: IO Int
part2 = do
  dirs <- splitOn "," . head . lines <$> readFile inputPath
  return . maximum $ distance <$> scanl move (0, 0) dirs
