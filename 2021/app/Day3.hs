module Day3 where

import Util

part1 :: IO ()
part1 = do
  xs <- fmap read . lines <$> input 3
  print (xs :: [Int])
