module Day1 where

import Util

part1 :: IO ()
part1 = do
  xs <- fmap read . lines <$> input 1
  print (xs :: [Int])
