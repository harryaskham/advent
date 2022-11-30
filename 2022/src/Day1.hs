module Day1 (part1, part2) where

import Data.List (maximum)
import Helper.TH
import Helper.Util
import Text.ParserCombinators.Parsec

elves :: Parser [[Integer]]
elves = many1 (number <* eol) `sepBy` eol

part1 :: Integer
part1 = $(input 1) & parseWith elves & fmap sum & maximum

part2 :: Integer
part2 = $(input 1) & parseWith elves & fmap sum & sortOn Down & take 3 & sum
