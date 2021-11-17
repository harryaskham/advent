module Day6 where

import Util

part1 :: IO ()
part1 = do
  xs <- fmap read . lines <$> input 6
  print (xs :: [Int])
