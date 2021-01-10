module TwentyEighteen.Day2 where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Data.Char
import Data.Function ((&))
import Data.List
import Data.List.Split
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Ord
import qualified Data.Sequence as SQ
import qualified Data.Set as S
import Data.Time
import qualified Data.Vector as V
import Text.ParserCombinators.ReadP
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()
import Util

-- Does the list contain the same thing exactly n times?
exactlyN :: Ord a => Int -> [a] -> Bool
exactlyN n xs = n `elem` (snd <$> M.toList (countMap xs))

part1 :: IO Int
part1 = do
  content <- readFile "input/2018/2.txt"
  return $ go 2 (lines content) * go 3 (lines content)
  where
    go :: Int -> [String] -> Int
    go n ls = length $ filter (exactlyN n) ls

-- Get the number of differences between the two strings.
editDistance :: Eq a => [a] -> [a] -> Int
editDistance xs ys = length $ filter (== False) $ fmap (uncurry (==)) (zip xs ys)

-- Gets only the items that are the same in both cases.
common :: Eq a => [a] -> [a] -> [a]
common xs ys =
  catMaybes $
    filter (/= Nothing) $ fmap (\(x, y) -> if x == y then Just x else Nothing) (zip xs ys)

part2 :: IO String
part2 = do
  ls <- lines <$> readFile "input/2018/2.txt"
  return $ head [common x y | x <- ls, y <- ls, editDistance x y == 1]
