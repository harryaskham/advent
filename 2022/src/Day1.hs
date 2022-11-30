module Day1 (part1, part2) where

import Data.List (maximum)
import Helper.TH (input)
import Helper.Util (eol, number, parseWith, (<$$>))
import Text.ParserCombinators.Parsec (Parser, many1, sepBy)

elves :: Parser [Integer]
elves = sum <$$> many1 (number <* eol) `sepBy` eol

part1 :: Integer
part1 = $(input 1) & parseWith elves & maximum

part2 :: Integer
part2 = $(input 1) & parseWith elves & sortOn Down & take 3 & sum
