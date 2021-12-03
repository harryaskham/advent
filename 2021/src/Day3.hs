module Day3 (part1, part2) where

import Data.Bits (Bits (complement))
import Helper.Bits (bitsToInt, leastCommonBit, mostCommonBit)
import Helper.Util (bitChar, both, eol, input, parseInput)
import Text.ParserCombinators.Parsec (GenParser, char, eof, many1)

parser :: GenParser Char () [[Bool]]
parser = many1 (many1 bitChar <* eol) <* eof

part1 :: IO Integer
part1 =
  uncurry (*)
    . both bitsToInt
    . (id &&& complement)
    . fmap mostCommonBit
    . transpose
    <$> parseInput parser (input 3)

step :: Int -> ([Bool] -> Bool) -> [[Bool]] -> [Bool]
step _ _ [bs] = bs
step i getCommonBit bss =
  step
    (i + 1)
    getCommonBit
    (filter (\bs -> (bs !!? i) == (getCommonBit <$> (transpose bss !!? i))) bss)

part2 :: IO Integer
part2 =
  uncurry (*)
    . both bitsToInt
    . (step 0 mostCommonBit &&& step 0 leastCommonBit)
    <$> parseInput parser (input 3)
