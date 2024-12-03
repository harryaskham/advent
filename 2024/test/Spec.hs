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
  describe "Day 1" do
    it "computes Part 1" $ Day1.part1 `shouldBe` 1941353
    it "computes Part 2" $ Day1.part2 `shouldBe` 22539317
  describe "Day 2" do
    it "computes Part 1" $ Day2.part1 `shouldBe` 524
    it "computes Part 2" $ Day2.part2 `shouldBe` 569
  describe "Day 3" do
    it "computes Part 1" $ Day3.part1 `shouldBe` 182780583
    it "computes Part 2" $ Day3.part2 `shouldBe` 90772405
