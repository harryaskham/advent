module Main where

import Day1 qualified
import Day10 qualified
import Day11 qualified
import Day12 qualified
import Day13 qualified
import Day14 qualified
import Day15 qualified
import Day16 qualified
import Day17 qualified
import Day18 qualified
import Day19 qualified
import Day2 qualified
import Day20 qualified
import Day21 qualified
import Day22 qualified
import Day23 qualified
import Day24 qualified
import Day25 qualified
import Day3 qualified
import Day4 qualified
import Day5 qualified
import Day6 qualified
import Day7 qualified
import Day8 qualified
import Day9 qualified
import Test.HUnit (assertEqual)

main :: IO ()
main = do
  assertEqual "Day 1 (Part 1)" 1316 Day1.part1
  assertEqual "Day 1 (Part 2)" 1344 Day1.part2

  assertEqual "Day 2 (Part 1)" 1690020 Day2.part1
  assertEqual "Day 2 (Part 2)" 1408487760 Day2.part2

  assertEqual "Day 3 (Part 1)" 3320834 Day3.part1
  assertEqual "Day 3 (Part 2)" 4481199 Day3.part2

  assertEqual "Day 4 (Part 1)" (Just 35670) Day4.part1
  assertEqual "Day 4 (Part 2)" (Just 22704) Day4.part2

  assertEqual "Day 5 (Part 1)" 6225 Day5.part1
  assertEqual "Day 5 (Part 2)" 22116 Day5.part2

  assertEqual "Day 6 (Part 1)" 377263 Day6.part1
  assertEqual "Day 6 (Part 2)" 1695929023803 Day6.part2

  assertEqual "Day 7 (Part 1)" 356992 Day7.part1
  assertEqual "Day 7 (Part 2)" 101268110 Day7.part2

  assertEqual "Day 8 (Part 1)" 488 Day8.part1
  assertEqual "Day 8 (Part 2)" 1040429 Day8.part2

  assertEqual "Day 9 (Part 1)" 465 Day9.part1
  assertEqual "Day 9 (Part 2)" 1269555 Day9.part2

  assertEqual "Day 10 (Part 1)" 392139 Day10.part1
  assertEqual "Day 10 (Part 2)" 4001832844 Day10.part2

  assertEqual "Day 11 (Part 1)" 1705 Day11.part1
  assertEqual "Day 11 (Part 2)" 265 Day11.part2

  assertEqual "Day 12 (Part 1)" 3485 Day12.part1
  assertEqual "Day 12 (Part 2)" 85062 Day12.part2

  assertEqual "Day 13 (Part 1)" "Part 1" Day13.part1
  assertEqual "Day 13 (Part 2)" "Part 2" Day13.part2

  assertEqual "Day 14 (Part 1)" "Part 1" Day14.part1
  assertEqual "Day 14 (Part 2)" "Part 2" Day14.part2

  assertEqual "Day 15 (Part 1)" "Part 1" Day15.part1
  assertEqual "Day 15 (Part 2)" "Part 2" Day15.part2

  assertEqual "Day 16 (Part 1)" "Part 1" Day16.part1
  assertEqual "Day 16 (Part 2)" "Part 2" Day16.part2

  assertEqual "Day 17 (Part 1)" "Part 1" Day17.part1
  assertEqual "Day 17 (Part 2)" "Part 2" Day17.part2

  assertEqual "Day 18 (Part 1)" "Part 1" Day18.part1
  assertEqual "Day 18 (Part 2)" "Part 2" Day18.part2

  assertEqual "Day 19 (Part 1)" "Part 1" Day19.part1
  assertEqual "Day 19 (Part 2)" "Part 2" Day19.part2

  assertEqual "Day 20 (Part 1)" "Part 1" Day20.part1
  assertEqual "Day 20 (Part 2)" "Part 2" Day20.part2

  assertEqual "Day 21 (Part 1)" "Part 1" Day21.part1
  assertEqual "Day 21 (Part 2)" "Part 2" Day21.part2

  assertEqual "Day 22 (Part 1)" "Part 1" Day22.part1
  assertEqual "Day 22 (Part 2)" "Part 2" Day22.part2

  assertEqual "Day 23 (Part 1)" "Part 1" Day23.part1
  assertEqual "Day 23 (Part 2)" "Part 2" Day23.part2

  assertEqual "Day 24 (Part 1)" "Part 1" Day24.part1
  assertEqual "Day 24 (Part 2)" "Part 2" Day24.part2

  assertEqual "Day 25 (Part 1)" "Part 1" Day25.part1
  assertEqual "Day 25 (Part 2)" "Part 2" Day25.part2
