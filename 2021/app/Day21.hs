module Day21 where

import Util

part1 :: IO ()
part1 = do
  xs <- fmap read . lines <$> input 21
  print (xs :: [Int])
