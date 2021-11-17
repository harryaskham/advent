module Day18 where

import Util

part1 :: IO ()
part1 = do
  xs <- fmap read . lines <$> input 18
  print (xs :: [Int])
