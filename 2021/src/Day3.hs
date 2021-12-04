module Day3 (part1, part2) where

import Data.Bits (Bits (complement))
import Helper.Bits (bitsToInt, leastCommonBit, mostCommonBit)
import Helper.TH (input)
import Helper.Util (bitChar, both, eol, parseWith)
import Text.ParserCombinators.Parsec (GenParser, char, eof, many1)

parser :: GenParser Char () [[Bool]]
parser = many1 (many1 bitChar <* eol) <* eof

filterBits :: ([Bool] -> Bool) -> [[Bool]] -> [Bool]
filterBits getCommonBit = go 0
  where
    go _ [bs] = bs
    go i bss =
      let cb = getCommonBit <$> (transpose bss !!? i)
       in go (i + 1) [bs | bs <- bss, (bs !!? i) == cb]

part1 :: Integer
part1 =
  $(input 3)
    & parseWith parser
    & transpose
    & fmap mostCommonBit
    & (id &&& complement)
    & both bitsToInt
    & uncurry (*)

part2 :: Integer
part2 =
  $(input 3)
    & parseWith parser
    & (filterBits mostCommonBit &&& filterBits leastCommonBit)
    & both bitsToInt
    & uncurry (*)
