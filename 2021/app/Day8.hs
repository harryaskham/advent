module Day8 where

import Util

part1 :: IO ()
part1 = do
  xs <- fmap read . lines <$> input 8
  print (xs :: [Int])
