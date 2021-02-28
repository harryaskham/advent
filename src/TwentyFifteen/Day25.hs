module TwentyFifteen.Day25 where

import Data.List (iterate')

part1 :: Int
part1 =
  snd . head . filter ((== (3018, 3009)) . fst) $
    zip
      (concat [[(x, y - x) | x <- [0 .. y]] | y <- [0 ..]])
      (iterate' ((`mod` 33554393) . (* 252533)) 20151125)
