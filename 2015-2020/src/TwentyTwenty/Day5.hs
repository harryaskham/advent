module TwentyTwenty.Day5 where

import qualified Data.Set as S

inputPath :: String
inputPath = "input/2020/5.txt"

seatCoords :: String -> (Int, Int)
seatCoords line = (x, y)
  where
    x = partition [0 .. 7] (drop 7 line)
    y = partition [0 .. 127] (take 7 line)

seatId :: (Int, Int) -> Int
seatId (x, y) = 8 * y + x

partition :: [Int] -> String -> Int
partition [x] [] = x
partition range (d : dirs) =
  case d of
    'L' -> lower
    'F' -> lower
    'R' -> upper
    'B' -> upper
  where
    lower = partition (take (length range `div` 2) range) dirs
    upper = partition (drop (length range `div` 2) range) dirs

part1 :: IO Int
part1 = do
  ls <- lines <$> readFile inputPath
  return $ maximum $ seatId . seatCoords <$> ls

part2 :: IO Int
part2 = do
  ls <- lines <$> readFile inputPath
  let allSeats = S.fromList [(x, y) | x <- [0 .. 7], y <- [1 .. 118]]
      passSeats = S.fromList $ seatCoords <$> ls
  return . seatId . head $ S.toList $ allSeats `S.difference` passSeats
