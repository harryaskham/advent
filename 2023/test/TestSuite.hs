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
  assertEqual "Day 1 (Part 1)" Day1.part1 54597
  assertEqual "Day 1 (Part 2)" Day1.part2 54504

  assertEqual "Day 2 (Part 1)" Day2.part1 2439
  assertEqual "Day 2 (Part 2)" Day2.part2 63711

  assertEqual "Day 3 (Part 1)" Day3.part1 553079
  assertEqual "Day 3 (Part 2)" Day3.part2 84363105

  assertEqual "Day 4 (Part 1)" Day4.part1 23028
  assertEqual "Day 4 (Part 2)" Day4.part2 9236992

  assertEqual "Day 5 (Part 1)" Day5.part1 836040384
  assertEqual "Day 5 (Part 2)" Day5.part2 10834440

  assertEqual "Day 6 (Part 1)" Day6.part1 1731600
  assertEqual "Day 6 (Part 2)" Day6.part2 40087680

  assertEqual "Day 7 (Part 1)" Day7.part1 250254244
  assertEqual "Day 7 (Part 2)" Day7.part2 250087440

  assertEqual "Day 8 (Part 1)" Day8.part1 18727
  assertEqual "Day 8 (Part 2)" Day8.part2 18024643846273

  assertEqual "Day 9 (Part 1)" Day9.part1 1681758908
  assertEqual "Day 9 (Part 2)" Day9.part2 803

  assertEqual "Day 10 (Part 1)" Day10.part1 6823
  assertEqual "Day 10 (Part 2)" Day10.part2 415

  assertEqual "Day 11 (Part 1)" Day11.part1 9370588
  assertEqual "Day 11 (Part 2)" Day11.part2 746207878188

  assertEqual "Day 12 (Part 1)" Day12.part1 7110
  assertEqual "Day 12 (Part 2)" Day12.part2 1566786613613

  assertEqual "Day 13 (Part 1)" Day13.part1 32035
  assertEqual "Day 13 (Part 2)" Day13.part2 24847

  assertEqual "Day 14 (Part 1)" Day14.part1 108840
  assertEqual "Day 14 (Part 2)" Day14.part2 103445

  assertEqual "Day 15 (Part 1)" Day15.part1 516657
  assertEqual "Day 15 (Part 2)" Day15.part2 210906

  assertEqual "Day 16 (Part 1)" Day16.part1 6605
  assertEqual "Day 16 (Part 2)" Day16.part2 6766

  assertEqual "Day 17 (Part 1)" Day17.part1 1256
  assertEqual "Day 17 (Part 2)" Day17.part2 1382

  assertEqual "Day 18 (Part 1)" Day18.part1 47045
  assertEqual "Day 18 (Part 2)" Day18.part2 147839570293376

  assertEqual "Day 19 (Part 1)" Day19.part1 342650
  assertEqual "Day 19 (Part 2)" Day19.part2 130303473508222

  assertEqual "Day 20 (Part 1)" Day20.part1 680278040
  assertEqual "Day 20 (Part 2)" Day20.part2 243548140870057

  assertEqual "Day 21 (Part 1)" Day21.part1 3532
  assertEqual "Day 21 (Part 2)" Day21.part2 590104708070703

  assertEqual "Day 22 (Part 1)" Day22.part1 439
  assertEqual "Day 22 (Part 2)" Day22.part2 43056

  assertEqual "Day 23 (Part 1)" Day23.part1 2202
  assertEqual "Day 23 (Part 2)" Day23.part2 6226

  assertEqual "Day 24 (Part 1)" Day24.part1 14046
  assertEqual "Day 24 (Part 2)" Day24.part2 808107741406756

  assertEqual "Day 25 (Part 1)" Day25.part1 525264
  assertEqual "Day 25 (Part 2)" Day25.part2 "Merry Christmas!"
