module Day15 where

import Util

part1 :: IO ()
part1 = do
  xs <- fmap read . lines <$> input 15
  print (xs :: [Int])
