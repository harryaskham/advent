{-# LANGUAGE MultiWayIf #-}

module TwentyEighteen.Day5 where

import Data.Char (toLower, toUpper)

-- Do two chars react with one another?
react :: Char -> Char -> Bool
react x y = x /= y && (toLower x == toLower y)

-- Reduces a polymer by exploding pairs of Aa, Bb etc. Only runs 1 step (e.g. needs applying recursively)
reducePolymer :: String -> String
reducePolymer "" = ""
reducePolymer [x] = [x]
reducePolymer (x : y : xs) =
  if
      | react x y -> reducePolymer xs
      | otherwise -> x : reducePolymer (y : xs)

-- Iterate reductioun until a fixed point.
reduceCompletely :: String -> String
reduceCompletely xs = if reducePolymer xs == xs then xs else reduceCompletely (reducePolymer xs)

part1 :: IO Int
part1 = do
  p <- head . lines <$> readFile "input/2018/5.txt"
  return $ length . reduceCompletely $ p

-- Gets a string without the given character
without :: Char -> String -> String
without c xs = [x | x <- xs, x /= toLower c, x /= toUpper c]

part2 :: IO Int
part2 = do
  p <- head . lines <$> readFile "input/2018/5.txt"
  return $ minimum $ length . reduceCompletely <$> (without <$> ['a' .. 'z'] <*> pure p)
