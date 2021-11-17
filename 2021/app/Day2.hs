module Day2 where

import Util

part1 :: IO ()
part1 = do
  xs <- fmap read . lines <$> input 2
  print (xs :: [Int])
