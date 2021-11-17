module Day12 where

import Util

part1 :: IO ()
part1 = do
  xs <- fmap read . lines <$> input 12
  print (xs :: [Int])
