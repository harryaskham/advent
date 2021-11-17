module Day16 where

import Util

part1 :: IO ()
part1 = do
  xs <- fmap read . lines <$> input 16
  print (xs :: [Int])
