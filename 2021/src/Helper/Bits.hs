{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Helper.Bits where

import Data.Bits
  ( Bits
      ( bit,
        bitSize,
        bitSizeMaybe,
        complement,
        isSigned,
        popCount,
        rotate,
        shift,
        testBit,
        xor,
        (.&.),
        (.|.)
      ),
  )
import Data.Semiring (Semiring, one, zero)
import Prelude hiding (one)

-- Inefficient but working interpretation of any list of zero/nonzero things as a bit list
-- zero interpreted as 0, one (or anything else) as 1
instance (Semiring a, Bits a) => Bits [a] where
  (a : as) .&. (b : bs) = (a .&. b) : (as .&. bs)
  _ .&. _ = []

  (a : as) .|. (b : bs) = (a .|. b) : (as .|. bs)
  _ .|. _ = []

  xor (a : as) (b : bs) = (a `xor` b) : (as `xor` bs)
  xor _ _ = []

  complement = fmap complement

  shift xs i
    | i > 0 = xs <> replicate i zero
    | i < 0 = take (bitSize xs - abs i) xs
    | otherwise = xs

  rotate xs i
    | i > 0 = drop (i `mod` bitSize xs) xs <> take (i `mod` bitSize xs) xs
    | i < 0 = drop (bitSize xs - (abs i `mod` bitSize xs)) xs <> take (bitSize xs - (abs i `mod` bitSize xs)) xs
    | otherwise = xs

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

-- True if true is the most common bit, False otherwise
-- Biases towards True in a tiebreak
mostCommonBit :: Bits a => a -> Bool
mostCommonBit bs
  | popCount bs == zeroCount bs = True
  | otherwise = popCount bs > zeroCount bs

-- False if false is the most common bit, True otherwise
-- Biases towards False in a tiebreak
leastCommonBit :: Bits a => a -> Bool
leastCommonBit bs
  | popCount bs == zeroCount bs = False
  | otherwise = popCount bs < zeroCount bs

hexToBinChars :: Char -> String
hexToBinChars '0' = "0000"
hexToBinChars '1' = "0001"
hexToBinChars '2' = "0010"
hexToBinChars '3' = "0011"
hexToBinChars '4' = "0100"
hexToBinChars '5' = "0101"
hexToBinChars '6' = "0110"
hexToBinChars '7' = "0111"
hexToBinChars '8' = "1000"
hexToBinChars '9' = "1001"
hexToBinChars 'A' = "1010"
hexToBinChars 'B' = "1011"
hexToBinChars 'C' = "1100"
hexToBinChars 'D' = "1101"
hexToBinChars 'E' = "1110"
hexToBinChars 'F' = "1111"
