module Day6 (part1, part2) where

import Data.List (nub)
import Data.Text qualified as T
import Helper.TH (input)
import Relude.Unsafe qualified as U

seqStart :: Int -> Text -> Integer
seqStart n as = go (fromIntegral n) $ T.unpack as
  where
    go c as
      | length (nub $ take n as) == n = c
      | otherwise = go (c + 1) (U.tail as)

part1 :: Integer
part1 = $(input 6) & seqStart 4

part2 :: Integer
part2 = $(input 6) & seqStart 14
