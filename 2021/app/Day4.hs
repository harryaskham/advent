module Day4 where

import Util

part1 :: IO ()
part1 = do
  xs <- fmap read . lines <$> input 4
  print (xs :: [Int])
