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
import Day666 qualified
import Day7 qualified
import Day8 qualified
import Day9 qualified
import Test.Hspec

-- $> import Test.Hspec  -- May be necessary for some setups.

-- $> hspec spec

spec :: Spec
spec = do
  describe "Day 1" do
    it "computes Part 1" $ Day1.part1 `shouldBe` 1941353
    it "computes Part 2" $ Day1.part2 `shouldBe` 22539317
  describe "Day 2" do
    it "computes Part 1" $ Day2.part1 `shouldBe` 524
    it "computes Part 2" $ Day2.part2 `shouldBe` 569
  describe "Day 3" do
    it "computes Part 1" $ Day3.part1 `shouldBe` 182780583
    it "computes Part 2" $ Day3.part2 `shouldBe` 90772405
  describe "Day 4" do
    it "computes Part 1" $ Day4.part1 `shouldBe` 2573
    it "computes Part 2" $ Day4.part2 `shouldBe` 1850
  describe "Day 5" do
    it "computes Part 1" $ Day5.part1 `shouldBe` (4462)
    it "computes Part 2" $ Day5.part2 `shouldBe` (6767)
  describe "Day 6" do
    it "computes Part 1" $ Day6.part1 `shouldBe` (Just 5095)
    it "computes Part 2" $ Day6.part2 `shouldBe` (Just 1933)
  describe "Day 7" do
    it "computes Part 1" $ Day7.part1 `shouldBe` (Σ 4998764814652)
    it "computes Part 2" $ Day7.part2 `shouldBe` (Σ 37598910447546)
  describe "Day 8" do
    it "computes Part 1" $ Day8.part1 `shouldBe` (Σ 254)
    it "computes Part 2" $ Day8.part2 `shouldBe` (Σ 951)
  describe "Day 9" do
    it "computes Part 1" $ Day9.part1 `shouldBe` 6430446922192
    it "computes Part 2" $ Day9.part2 `shouldBe` 6460170593016
  describe "Day 10" do
    it "computes Part 1" $ Day10.part1 `shouldBe` (Σ 659)
    it "computes Part 2" $ Day10.part2 `shouldBe` (Σ 1463)
  describe "Day 11" do
    it "computes Part 1" $ Day11.part1 `shouldBe` 183484
    it "computes Part 2" $ Day11.part2 `shouldBe` 218817038947400
  describe "Day 12" do
    it "computes Part 1" $ Day12.part1 `shouldBe` 1456082
    it "computes Part 2" $ Day12.part2 `shouldBe` 872382
  describe "Day 13" do
    it "computes Part 1" $ Day13.part1 `shouldBe` (Σ 28059)
    it "computes Part 2" $ Day13.part2 `shouldBe` (Σ 102255878088512)
  describe "Day 14" do
    it "computes Part 1" $ Day14.part1 `shouldBe` 215476074
    it "computes Part 2" $ Day14.part2 `shouldBe` 6285
  describe "Day 15" do
    it "computes Part 1" $ Day15.part1 `shouldBe` (Σ 1448589)
    it "computes Part 2" $ Day15.part2 `shouldBe` (Σ 1472235)
  describe "Day 16" do
    it "computes Part 1" $ Day16.part1 `shouldBe` 143564
    it "computes Part 2" $ Day16.part2 `shouldBe` 593
  describe "Day 17" do
    it "computes Part 1" $ Day17.part1 `shouldBe` "1,3,5,1,7,2,5,1,6"
    it "computes Part 2" $ Day17.part2 `shouldBe` 236555997372013
  describe "Day 18" do
    it "computes Part 1" $ Day18.part1 `shouldBe` 304
    it "computes Part 2" $ Day18.part2 `shouldBe` "50,28"
  describe "Day 19" do
    it "computes Part 1" $ Day19.part1 `shouldBe` (Σ 285)
    it "computes Part 2" $ Day19.part2 `shouldBe` (Σ 636483903099279)
  describe "Day 20" do
    it "computes Part 1" $ Day20.part1 `shouldBe` 1381
    it "computes Part 2" $ Day20.part2 `shouldBe` 982124
  describe "Day 21" do
    it "computes Part 1" $ Day21.part1 `shouldBe` 134120
    it "computes Part 2" $ Day21.part2 `shouldBe` 167389793580400
  describe "Day 666" do
    it "computes Part 1" $ Day666.part1 `shouldBe` "Part 1"
    it "computes Part 2" $ Day666.part2 `shouldBe` "Part 2"

main :: IO ()
main = hspec spec
