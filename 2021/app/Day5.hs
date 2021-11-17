module Day5 where

import Util

part1 :: IO ()
part1 = do
  xs <- fmap read . lines <$> input 5
  print (xs :: [Int])
