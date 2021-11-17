module Day19 where

import Util

part1 :: IO ()
part1 = do
  xs <- fmap read . lines <$> input 19
  print (xs :: [Int])
