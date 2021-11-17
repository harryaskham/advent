module Day13 where

import Util

part1 :: IO ()
part1 = do
  xs <- fmap read . lines <$> input 13
  print (xs :: [Int])
