{-# LANGUAGE BangPatterns #-}

module TwentySeventeen.Day17 where

import Data.List (foldl')
import qualified Data.Sequence as SQ
import Data.Tuple.Extra (thd3)

offset :: Int
offset = 382

step :: (Int, SQ.Seq Int) -> (Int, SQ.Seq Int)
step (current, ring) = (next, ring')
  where
    next = (current + offset + 1) `mod` length ring
    ring' = SQ.insertAt next (length ring) ring

part1 :: Int
part1 =
  let (i, ring) = iterate step (0, SQ.singleton 0) !! 2017
   in ring `SQ.index` ((i + 1) `mod` length ring)

part2 :: Int
part2 = thd3 $ foldl' go (0, 1, 0) [1 .. 50000000]
  where
    go (!current, !size, !rightOfZero) _ =
      ( (current + offset + 1) `mod` (size + 1),
        size + 1,
        if current == 0 then size else rightOfZero
      )
