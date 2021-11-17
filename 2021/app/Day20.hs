module Day20 where

import Util

part1 :: IO ()
part1 = do
  xs <- fmap read . lines <$> input 20
  print (xs :: [Int])
