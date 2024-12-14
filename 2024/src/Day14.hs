module Day14 (part1, part2) where

dims :: ℤ²
dims = (101, 103)

robots :: [ℤ⁴]
robots = $(aocx 14) |-. tuples @4 (numbers @ℤ)

part1 = robots

part2 :: ℤ
part2 = 0
