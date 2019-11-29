module TwentyEighteen where

import qualified Data.Set as S
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe

-- Read signed ints from file.
freqsToNums :: IO [Int]
freqsToNums = do
  content <- readFile "input/2018/1.txt"
  return $ parseLine <$> lines content
    where
      parseLine :: String -> Int
      parseLine (sign:number) =
        case sign of
          '+' -> read number
          '-' -> -1 * read number

day1_1 :: IO Int
day1_1 = do
  nums <- freqsToNums
  return $ sum nums

day1_2 :: IO Int
day1_2 = do
  nums <- cycle <$> freqsToNums
  return $ next S.empty nums 0
    where
      next :: S.Set Int -> [Int] -> Int -> Int
      next seen (n:ns) frequency =
        if frequency `S.member` seen then frequency else next (S.insert frequency seen) ns (frequency + n)

-- Get a count of unique items in a list.
itemCounts :: Ord a => [a] -> M.Map a Int
itemCounts xs = go M.empty xs
  where
    go :: Ord a => M.Map a Int -> [a] -> M.Map a Int
    go acc [] = acc
    go acc (x:xs) = go (M.insertWith (+) x 1 acc) xs

-- Does the list contain the same thing exactly n times?
exactlyN :: Ord a => Int -> [a] -> Bool
exactlyN n xs = n `elem` (snd <$> M.toList (itemCounts xs))

day2_1 :: IO Int
day2_1 = do
  content <- readFile "input/2018/2.txt"
  return $ go 2 (lines content) * go 3 (lines content)
    where
      go :: Int -> [String] -> Int
      go n ls = length $ filter (exactlyN n) ls

-- Get the number of differences between the two strings.
editDistance :: Eq a => [a] -> [a] -> Int
editDistance xs ys = length $ filter (==False) $ fmap (\(x, y) -> x == y) (zip xs ys)

-- Gets only the items that are the same in both cases.
common :: Eq a => [a] -> [a] -> [a]
common xs ys = catMaybes $ filter (/=Nothing) $ fmap (\(x, y) -> if x == y then Just x else Nothing) (zip xs ys)

day2_2 :: IO String
day2_2 = do
  ls <- lines <$> readFile "input/2018/2.txt"
  return $ [common x y | x <- ls, y <- ls, editDistance x y == 1] !! 0
