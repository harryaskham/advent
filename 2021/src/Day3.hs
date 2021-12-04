module Day3 (part1, part2) where

import Data.Bits (Bits (complement))
import Helper.Bits (bitsToInt, leastCommonBit, mostCommonBit)
import Helper.TH (inputL)
import Helper.Util (bitChar, both, eol, parseInputL)
import Text.ParserCombinators.Parsec (GenParser, char, eof, many1)

parser :: GenParser Char () [[Bool]]
parser = many1 (many1 bitChar <* eol) <* eof

part1 :: Integer
part1 =
  uncurry (*)
    . both bitsToInt
    . (id &&& complement)
    . fmap mostCommonBit
    . transpose
    $ parseInputL parser $(inputL 3)

filterBits :: ([Bool] -> Bool) -> [[Bool]] -> [Bool]
filterBits getCommonBit = go 0
  where
    go _ [bs] = bs
    go i bss =
      let cb = getCommonBit <$> (transpose bss !!? i)
       in go (i + 1) [bs | bs <- bss, (bs !!? i) == cb]

part2 :: Integer
part2 =
  uncurry (*)
    . both bitsToInt
    . (filterBits mostCommonBit &&& filterBits leastCommonBit)
    $ parseInputL parser $(inputL 3)
