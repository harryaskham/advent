module Day1 (part1, part2) where

import Control.Arrow ((***))

lists :: ([Int], [Int])
lists = $(input 1) |-.. twoOf (spaceTabs `surrounding` number @Int) & unzip

part1 :: Int
part1 =
  lists
    & both sort
    & uncurry (zipWith ((abs .) . (-)))
    & sum

part2 :: Int
part2 =
  lists
    & second countMap
    &<@> (bicomp . ((*) &&& ((? 0) .<. flip (|?))))
    & sum
