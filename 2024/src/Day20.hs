module Day20 (part1, part2) where

part1 :: Text
part1 =
  $(aoc 20)
    -- & readAs (signed decimal)
    -- & parseWith parser
    -- & parseLinesWith line
    -- & lines
    -- & readGrid
    & (<> "Part 1")

part2 :: Text
part2 = "Part 2"
