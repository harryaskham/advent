module Day3 (part1, part2) where

import Data.Bits (Bits (complement))
import Helper.Bits (bitsToInt, leastCommonBit, mostCommonBit)
import Helper.TH (input)
import Helper.Util (bitChar, both, parseLinesWith)
import Text.ParserCombinators.Parsec (GenParser, many1)

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
    & parseLinesWith (many1 bitChar)
    & transpose
    & fmap mostCommonBit
    & (id &&& complement)
    & both bitsToInt
    & uncurry (*)

part2 :: Integer
part2 =
  $(input 3)
    & parseLinesWith (many1 bitChar)
    & (filterBits mostCommonBit &&& filterBits leastCommonBit)
    & both bitsToInt
    & uncurry (*)
