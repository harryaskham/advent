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

instance (Semiring a, Bits a) => Bits [a] where
  (a : as) .&. (b : bs) = (a .&. b) : (as .&. bs)
  _ .&. _ = []

  (a : as) .|. (b : bs) = (a .|. b) : (as .|. bs)
  _ .|. _ = []

  xor (a : as) (b : bs) = (a `xor` b) : (as `xor` bs)
  xor _ _ = []

  complement = fmap complement

  -- TODO: Implement
  shift = undefined

  -- TODO: Implement
  rotate = undefined

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
