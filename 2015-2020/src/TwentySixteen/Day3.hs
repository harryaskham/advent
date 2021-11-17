module TwentySixteen.Day3 where

import Data.List (transpose)
import Data.List.Split (chunksOf)
import Util ((<$$>))

inputPath :: String
inputPath = "input/2016/3.txt"

triangles :: IO [[Int]]
triangles = (read <$$>) . (words <$>) . lines <$> readFile inputPath

valid :: [Int] -> Bool
valid [a, b, c] = a + b > c && a + c > b && b + c > a

part1 :: IO Int
part1 = length . filter valid <$> triangles

part2 :: IO Int
part2 = length . filter valid . concatMap (chunksOf 3) . transpose <$> triangles
