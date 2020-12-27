module TwentySeventeen.Day13 where

import Data.List.Split

inputPath :: String
inputPath = "input/2017/13.txt"

type Depth = Int

type Range = Int

data Layer = Layer Depth Range deriving (Show)

readInput :: IO [Layer]
readInput = do
  ls <- lines <$> readFile inputPath
  return $ (\[d, l] -> Layer (read d) (read l)) <$> (splitOn ": " <$> ls)

hitsLayer :: Int -> Layer -> Bool
hitsLayer delay (Layer depth range) = (depth + delay) `mod` (2 * (range - 1)) == 0

score :: Layer -> Int
score (Layer depth range) = depth * range

part1 :: IO Int
part1 = sum . fmap score . filter (hitsLayer 0) <$> readInput

part2 :: IO Int
part2 = do
  layers <- readInput
  return $ head [d | d <- [0 ..], not . or $ hitsLayer d <$> layers]
