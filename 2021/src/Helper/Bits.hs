{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Helper.Bits where

import Data.Array.BitArray (BitArray)
import Data.Array.BitArray qualified as BA
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

hexToBin :: Char -> String
hexToBin '0' = "0000"
hexToBin '1' = "0001"
hexToBin '2' = "0010"
hexToBin '3' = "0011"
hexToBin '4' = "0100"
hexToBin '5' = "0101"
hexToBin '6' = "0110"
hexToBin '7' = "0111"
hexToBin '8' = "1000"
hexToBin '9' = "1001"
hexToBin 'A' = "1010"
hexToBin 'B' = "1011"
hexToBin 'C' = "1100"
hexToBin 'D' = "1101"
hexToBin 'E' = "1110"
hexToBin 'F' = "1111"
hexToBin x = error ("Invalid hex: " <> show x)

bitArrayToInt :: BitArray Int -> Int
bitArrayToInt ba =
  let (_, l) = BA.bounds ba
   in sum [2 ^ (l - i) | (i, b) <- BA.assocs ba, b]
