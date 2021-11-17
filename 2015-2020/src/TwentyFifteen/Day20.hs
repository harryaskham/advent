module TwentyFifteen.Day20 where

import Data.List (nub)

target :: Int
target = 34000000

presentsAtHouseNumber :: Int -> Int
presentsAtHouseNumber n =
  (10 *) . sum . nub . concat $
    [ [a, n `div` a]
      | a <- n : [1 .. floor . sqrt . fromIntegral $ n],
        n `rem` a == 0
    ]

solve :: (Int -> Int) -> Int
solve f = fst . head . dropWhile ((< target) . snd) $ [(n, f n) | n <- [1 ..]]

part1 :: Int
part1 = solve presentsAtHouseNumber

presentsAtHouseNumber2 :: Int -> Int
presentsAtHouseNumber2 n =
  (11 *) . sum . nub . concat $
    [ [x | x <- [a, n `div` a], n `div` x <= 50]
      | a <- n : [1 .. floor . sqrt . fromIntegral $ n],
        n `rem` a == 0
    ]

part2 :: Int
part2 = solve presentsAtHouseNumber2
