module Day6 (part1, part2) where

import Data.List (nub)
import Data.Text qualified as T
import Helper.TH (input)

seqStart :: Int -> Text -> Maybe Integer
seqStart n as = go (fromIntegral n) $ T.unpack as
  where
    go c as@(_ : rest)
      | length (nub $ take n as) == n = Just c
      | otherwise = go (c + 1) rest
    go _ _ = Nothing

part1 :: Maybe Integer
part1 = $(input 6) & seqStart 4

part2 :: Maybe Integer
part2 = $(input 6) & seqStart 14
