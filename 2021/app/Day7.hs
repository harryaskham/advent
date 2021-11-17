module Day7 where

import Util

part1 :: IO ()
part1 = do
  xs <- fmap read . lines <$> input 7
  print (xs :: [Int])
