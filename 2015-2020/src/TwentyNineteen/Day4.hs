module TwentyNineteen.Day4 where

import Data.Char (digitToInt)

hasTwoAdjacent :: Int -> Bool
hasTwoAdjacent x = go $ show x
  where
    go :: String -> Bool
    go [] = False
    go [_] = False
    go (a : b : xs) = (a == b) || go (b : xs)

hasMonotonicDigits :: Int -> Bool
hasMonotonicDigits x = go $ show x
  where
    go :: String -> Bool
    go [] = True
    go [_] = True
    go (a : b : xs) = (digitToInt b >= digitToInt a) && go (b : xs)

part1 :: Int
part1 = length [x | x <- [265275 .. 781584], hasTwoAdjacent x, hasMonotonicDigits x]

hasPreciselyTwoAdjacent :: Int -> Bool
hasPreciselyTwoAdjacent x = go $ show x
  where
    go :: String -> Bool
    go [a, b, c, d, e, f] =
      or
        [ (a == b) && (b /= c),
          (a /= b) && (b == c) && (c /= d),
          (b /= c) && (c == d) && (d /= e),
          (c /= d) && (d == e) && (e /= f),
          (d /= e) && (e == f)
        ]

part2 :: Int
part2 = length [x | x <- [265275 .. 781584], hasPreciselyTwoAdjacent x, hasMonotonicDigits x]
