module Spec where

import Data.Vector qualified as V
import Day1 qualified
import Day10 qualified
import Day11 qualified
import Day12 qualified
import Day2 qualified
import Day3 qualified
import Day4 qualified
import Day5 qualified
import Day6 qualified
import Day7 qualified
import Day8 qualified
import Day9 qualified
import Test.Hspec

main :: IO ()
main = hspec do
  describe ("Day 1") do
    it "computes Part 1" (Day1.part1 `shouldBe` 1129)
    it "computes Part 2" (Day1.part2 `shouldBe` 6638)
  describe ("Day 2") do
    it "computes Part 1" (Day2.part1 `shouldBe` Σ 44487518055)
    it "computes Part 2" (Day2.part2 `shouldBe` Σ 4301558612814388)
  describe ("Day 2") do
    it "computes Part 1" (Day3.part1 `shouldBe` 17158)
    it "computes Part 2" (Day3.part2 `shouldBe` 170449335646486)
