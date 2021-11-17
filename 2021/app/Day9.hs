module Day9 where

import Util

part1 :: IO ()
part1 = do
  xs <- fmap read . lines <$> input 9
  print (xs :: [Int])
