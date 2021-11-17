module Day14 where

import Util

part1 :: IO ()
part1 = do
  xs <- fmap read . lines <$> input 14
  print (xs :: [Int])
