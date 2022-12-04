module Day4 (part1, part2) where

import Helper.TH (input)
import Helper.Util (eol, number, parseWith, toTuple2, (<$$>))
import Text.ParserCombinators.Parsec (Parser, char, eof, many1, sepBy)

parser :: Parser [((Int, Int), (Int, Int))]
parser = many1 (line <* eol) <* eof
  where
    range = number `sepBy` char '-'
    line = toTuple2 <$> toTuple2 <$$> range `sepBy` char ','

subsumed :: ((Int, Int), (Int, Int)) -> Bool
subsumed ((a, b), (c, d)) = (a >= c && b <= d) || (c >= a && d <= b)

overlap :: ((Int, Int), (Int, Int)) -> Bool
overlap ((a, b), (c, d)) = (c <= b && c >= a) || (a <= d && a >= c)

part1 :: Int
part1 = $(input 4) & parseWith parser & filter subsumed & length

part2 :: Int
part2 = $(input 4) & parseWith parser & filter overlap & length
