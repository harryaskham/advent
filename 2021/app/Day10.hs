module Day10 where

import Util

part1 :: IO ()
part1 = do
  xs <- fmap read . lines <$> input 10
  print (xs :: [Int])
