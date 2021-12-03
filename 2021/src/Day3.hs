module Day3 (part1, part2) where

import Data.Bits (Bits (complement))
import Helper.Bits (bitsToInt, leastCommonBit, mostCommonBit)
import Helper.Util (bitChar, eol, input, parseInput)
import Text.ParserCombinators.Parsec (GenParser, char, eof, many1)

parser :: GenParser Char () [[Bool]]
parser = many1 (many1 bitChar <* eol) <* eof

part1 :: IO Integer
part1 =
  (uncurry (*) . (bitsToInt &&& (bitsToInt . complement)))
    . fmap mostCommonBit
    . transpose
    <$> parseInput parser (input 3)

step :: [[Bool]] -> Int -> ([Bool] -> Bool) -> [Bool]
step [bs] _ _ = bs
step bss i getBit = step (filter (\bs -> (bs !!? i) == m) bss) (i + 1) getBit
  where
    m = getBit <$> (transpose bss !!? i)

part2 :: IO Integer
part2 = do
  xs <- parseInput parser (input 3)
  return . product $ bitsToInt . step xs 0 <$> [mostCommonBit, leastCommonBit]
