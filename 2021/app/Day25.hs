module Day25 where

import Util

part1 :: IO ()
part1 = do
  xs <- fmap read . lines <$> input 25
  print (xs :: [Int])
