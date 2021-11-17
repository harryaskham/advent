module Day11 where

import Util

part1 :: IO ()
part1 = do
  xs <- fmap read . lines <$> input 11
  print (xs :: [Int])
