module Spec where

import Data.Vector qualified as V
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
import Test.Hspec

main :: IO ()
main = hspec $ do
  let parts =
        mkVec
          [ (Day1.part1, Day1.part2),
            (Day2.part1, Day2.part2),
            (Day3.part1, Day3.part2),
            (Day4.part1, Day4.part2),
            (Day5.part1, Day5.part2),
            (Day6.part1, Day6.part2),
            (Day7.part1, Day7.part2),
            (Day8.part1, Day8.part2),
            (Day9.part1, Day9.part2),
            (Day10.part1, Day10.part2),
            (Day11.part1, Day11.part2),
            (Day12.part1, Day12.part2),
            (Day13.part1, Day13.part2),
            (Day14.part1, Day14.part2),
            (Day15.part1, Day15.part2),
            (Day16.part1, Day16.part2),
            (Day17.part1, Day17.part2),
            (Day18.part1, Day18.part2),
            (Day19.part1, Day19.part2),
            (Day20.part1, Day20.part2),
            (Day21.part1, Day21.part2),
            (Day22.part1, Day22.part2),
            (Day23.part1, Day23.part2),
            (Day24.part1, Day24.part2),
            (Day25.part1, Day25.part2)
          ]
  forM_ [1 .. 25] $ \day ->
    describe ("Day " <> show day) do
      let part = (parts V.! (day - 1))
      it "computes Part 1" do
        fst part `shouldBe` "Part 1"
      it "computes Part 2" do
        snd part `shouldBe` "Part 2"
