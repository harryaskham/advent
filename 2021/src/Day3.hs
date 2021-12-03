{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Day3 (part1, part2, bitsToInt, mostCommonBit) where

import Data.Array qualified as A
import Data.Bimap (Bimap)
import Data.Bimap qualified as BM
import Data.Bits
import Data.Map.Strict qualified as M
import Data.Mod
import Data.PQueue.Prio.Min qualified as PQ
import Data.Semiring
import Data.Sequence qualified as SQ
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Read
import Data.Vector qualified as V
import GHC.Real (Integral (toInteger))
import Helper.Coord
import Helper.Grid
import Helper.Tracers
import Helper.Util
import Text.ParserCombinators.Parsec hiding ((<|>))
import Prelude hiding (one, sum, (*), (+), (-), (^))

instance (Semiring a, Bits a) => Bits [a] where
  (a : as) .&. (b : bs) = (a .&. b) : (as .&. bs)
  _ .&. _ = []

  (a : as) .|. (b : bs) = (a .|. b) : (as .|. bs)
  _ .|. _ = []

  xor (a : as) (b : bs) = (a `xor` b) : (as `xor` bs)
  xor _ _ = []

  complement = fmap complement
  shift i xs = undefined
  rotate i xs = undefined
  bitSize = length
  bitSizeMaybe = const Nothing
  isSigned = const False
  testBit xs i =
    case xs !!? i of
      Nothing -> False
      Just x -> x /= zero

  bit i = one : replicate i zero
  popCount xs = length [i | i <- [0 .. length xs - 1], testBit xs i]

bitsToInt :: Bits a => a -> Integer
bitsToInt bs =
  sum $ (\i -> if testBit bs (bitSize bs - i - 1) then 2 ^ i else 0) <$> [0 .. bitSize bs - 1]

zeroCount :: Bits a => a -> Int
zeroCount bs = bitSize bs - popCount bs

mostCommonBit :: Bits a => a -> Bool
mostCommonBit bs
  | popCount bs == zeroCount bs = True
  | otherwise = popCount bs > zeroCount bs

leastCommonBit :: Bits a => a -> Bool
leastCommonBit bs
  | popCount bs == zeroCount bs = False
  | otherwise = popCount bs < zeroCount bs

parser :: GenParser Char () [[Bool]]
parser = many1 (many1 bitChar <* eol) <* eof
  where
    bitChar = (char '1' >> return True) <|> (char '0' >> return False)

part1 :: IO Integer
part1 = do
  xs <- parseInput parser (input 3)
  let gamma = mostCommonBit <$> transpose xs
      epsilon = complement gamma
  return $ bitsToInt gamma * bitsToInt epsilon

step :: ([Bool] -> Bool) -> [[Bool]] -> Int -> [Bool]
step getBit [bs] _ = bs
step getBit bss i = step getBit (filter (\bs -> (bs !!? i) == m) bss) (i + 1)
  where
    m = getBit <$> (transpose bss !!? i)

part2 :: IO Integer
part2 = do
  xs <- parseInput parser (input 3)
  let oxygen = step mostCommonBit xs 0
  let co2 = step leastCommonBit xs 0
  return $ bitsToInt oxygen * bitsToInt co2
