module TwentyTwenty.Day10 where

import Control.Monad.Memo (Memo, MonadMemo (memo), startEvalMemo)
import Data.List (sort)

inputPath :: String
inputPath = "input/2020/10.txt"

differences :: Int -> Int -> Int -> [Int] -> Int
differences _ ones threes [] = ones * (threes + 1)
differences last ones threes (j : js)
  | j - last == 1 = differences j (ones + 1) threes js
  | j - last == 3 = differences j ones (threes + 1) js
  | otherwise = error "Impossible input"

part1 :: IO Int
part1 = differences 0 0 0 . sort . fmap read . lines <$> readFile inputPath

arrangements :: (Int, [Int]) -> Memo (Int, [Int]) Int Int
arrangements (_, [_]) = return 1
arrangements (last, j1 : j2 : js) = do
  nWithoutSkip <- memo arrangements (j1, j2 : js)
  nWithSkip <- memo arrangements (last, j2 : js)
  if j2 - last > 3
    then return nWithoutSkip
    else return $ nWithoutSkip + nWithSkip

part2 :: IO Int
part2 = do
  js <- sort . fmap read . lines <$> readFile inputPath
  return $ startEvalMemo (arrangements (0, js))
