module DayX where

import Data.List (nub)
import Util

part1 :: IO ()
part1 = do
  xs <- fmap read . lines <$> input X
  print (xs :: [Int])
