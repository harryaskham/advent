module Day24 where

import Util

part1 :: IO ()
part1 = do
  xs <- fmap read . lines <$> input 24
  print (xs :: [Int])
