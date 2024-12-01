module Day1 (part1, part2) where

lists :: ([Int], [Int])
lists = $(input 1) |-.. twoOf (spaceTabs `surrounding` number @Int) & unzip

part1 :: Int
part1 = sum $ sort `both` lists &+> (abs .) . (-)

part2 :: Int
part2 =
  second countMap lists
    &<@> (bicomp . ((*) &&& ((? 0) .<. flip (|?))))
    & sum
