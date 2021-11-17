module Day17 where

import Util (input)

part1 :: IO ()
part1 = do
  xs <- fmap read . lines <$> input 17
  print (xs :: [Int])
