module Day17 (part1, part2) where

import Data.List qualified as L

data Target = Target
  { minX :: Int,
    maxX :: Int,
    minY :: Int,
    maxY :: Int
  }

target :: Target
target = Target 34 67 (-215) (-186)

step :: (Int, Int) -> Maybe Int -> (Int, Int) -> Maybe Int
step (x, y) peak (vx, vy)
  | x > maxX target = Nothing
  | vx == 0 && y < minY target = Nothing
  | y < minY target = Nothing
  | x >= minX target
      && x <= maxX target
      && y >= minY target
      && y <= maxY target =
    peak
  | otherwise =
    step
      (x + vx, y + vy)
      (if Just y > peak then Just y else peak)
      (if vx > 0 then vx - 1 else if vx < 0 then vx + 1 else 0, vy - 1)

peaks :: [Int]
peaks =
  mapMaybe
    (step (0, 0) Nothing)
    [ (vx, vy)
      | vx <- [1 .. maxX target + 1],
        vy <- [minY target .. 400]
    ]

part1 :: Int
part1 = L.maximum peaks

part2 :: Int
part2 = length peaks
