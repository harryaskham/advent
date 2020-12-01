module TwentyTwenty.Day1 where

import Data.List (nub)
import Data.Maybe (catMaybes)

inputPath :: String
inputPath = "input/2020/1.txt"

-- Take an indexed pair and return the product if valid.
solvePair :: (Int, Int) -> (Int, Int) -> Maybe Int
solvePair (i1, e1) (i2, e2) =
  if i1 /= i2 && e1 + e2 == 2020
    then Just (e1 * e2)
    else Nothing

part1 :: IO ()
part1 = do
  ls <- lines <$> readFile inputPath
  let entries = zip [0 ..] $ read <$> ls
  -- Solve all pairs of indexed entries, and remove duplication
  print $ head $ nub $ catMaybes $ solvePair <$> entries <*> entries

-- Combine two Maybes, dropping second one if both are Just.
combineMaybe :: Maybe a -> Maybe a -> Maybe a
combineMaybe (Just a) (Just _) = Just a
combineMaybe (Just a) Nothing = Just a
combineMaybe Nothing (Just a) = Just a
combineMaybe Nothing Nothing = Nothing

-- Find the product of the subset of length n that sums to 2020
findProduct :: [Int] -> [Int] -> Int -> Int -> Maybe Int
findProduct [] _ _ _ = Nothing
findProduct (x : xs) used n target
  | length used == n && sum used == target = Just $ product used
  | length used == n = Nothing
  | otherwise =
    combineMaybe
      (findProduct xs used n target)
      (findProduct xs (x : used) n target)

part2 = do
  ls <- lines <$> readFile inputPath
  let entries = read <$> ls
  print $ findProduct entries [] 3 2020
