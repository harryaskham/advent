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
  assertEqual "Day 1 (Part 1)" Day1.part1 71023
  assertEqual "Day 1 (Part 2)" Day1.part2 206289

  assertEqual "Day 2 (Part 1)" Day2.part1 15523
  assertEqual "Day 2 (Part 2)" Day2.part2 15702

  assertEqual "Day 3 (Part 1)" Day3.part1 7428
  assertEqual "Day 3 (Part 2)" Day3.part2 2650

  assertEqual "Day 4 (Part 1)" Day4.part1 526
  assertEqual "Day 4 (Part 2)" Day4.part2 886

  assertEqual "Day 5 (Part 1)" Day5.part1 "CWMTGHBDW"
  assertEqual "Day 5 (Part 2)" Day5.part2 "SSCGWJCRB"

  assertEqual "Day 6 (Part 1)" Day6.part1 (Just 1238)
  assertEqual "Day 6 (Part 2)" Day6.part2 (Just 3037)

  assertEqual "Day 7 (Part 1)" Day7.part1 1391690
  assertEqual "Day 7 (Part 2)" Day7.part2 5469168

  assertEqual "Day 8 (Part 1)" Day8.part1 1693
  assertEqual "Day 8 (Part 2)" Day8.part2 422059

  assertEqual "Day 9 (Part 1)" Day9.part1 6494
  assertEqual "Day 9 (Part 2)" Day9.part2 2691

  assertEqual "Day 10 (Part 1)" Day10.part1 15680
  assertEqual "Day 10 (Part 2)" Day10.part2 "#### #### ###  #### #  #  ##  #  # ### \n   # #    #  # #    #  # #  # #  # #  #\n  #  ###  ###  ###  #### #    #  # #  #\n #   #    #  # #    #  # # ## #  # ### \n#    #    #  # #    #  # #  # #  # #   \n#### #    ###  #    #  #  ###  ##  #   "

  assertEqual "Day 11 (Part 1)" Day11.part1 64032
  assertEqual "Day 11 (Part 2)" Day11.part2 12729522272

  assertEqual "Day 12 (Part 1)" Day12.part1 (Just 472)
  assertEqual "Day 12 (Part 2)" Day12.part2 (Just 465)

  assertEqual "Day 13 (Part 1)" Day13.part1 5208
  assertEqual "Day 13 (Part 2)" Day13.part2 25792

  assertEqual "Day 14 (Part 1)" Day14.part1 737
  assertEqual "Day 14 (Part 2)" Day14.part2 28145

  assertEqual "Day 15 (Part 1)" Day15.part1 5299855
  assertEqual "Day 15 (Part 2)" Day15.part2 13615843289729

  assertEqual "Day 16 (Part 1)" Day16.part1 1792
  assertEqual "Day 16 (Part 2)" Day16.part2 2587

  assertEqual "Day 17 (Part 1)" Day17.part1 3153
  assertEqual "Day 17 (Part 2)" Day17.part2 1553665689155

  assertEqual "Day 18 (Part 1)" Day18.part1 3454
  assertEqual "Day 18 (Part 2)" Day18.part2 2014

  assertEqual "Day 19 (Part 1)" Day19.part1 "Part 1"
  assertEqual "Day 19 (Part 2)" Day19.part2 "Part 2"

  assertEqual "Day 20 (Part 1)" Day20.part1 "Part 1"
  assertEqual "Day 20 (Part 2)" Day20.part2 "Part 2"

  assertEqual "Day 21 (Part 1)" Day21.part1 "Part 1"
  assertEqual "Day 21 (Part 2)" Day21.part2 "Part 2"

  assertEqual "Day 22 (Part 1)" Day22.part1 "Part 1"
  assertEqual "Day 22 (Part 2)" Day22.part2 "Part 2"

  assertEqual "Day 23 (Part 1)" Day23.part1 "Part 1"
  assertEqual "Day 23 (Part 2)" Day23.part2 "Part 2"

  assertEqual "Day 24 (Part 1)" Day24.part1 "Part 1"
  assertEqual "Day 24 (Part 2)" Day24.part2 "Part 2"

  assertEqual "Day 25 (Part 1)" Day25.part1 "Part 1"
  assertEqual "Day 25 (Part 2)" Day25.part2 "Part 2"
