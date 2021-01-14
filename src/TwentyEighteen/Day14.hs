module TwentyEighteen.Day14 where

import Data.Char (digitToInt, intToDigit)
import qualified Data.Foldable as F
import Data.Sequence (Seq)
import qualified Data.Sequence as SQ

type ElfState = (Int, Int, Seq Int)

mkElfState :: ElfState
mkElfState = (0, 1, SQ.fromList [3, 7])

step :: ElfState -> ElfState
step (ix1, ix2, rs) = (newIx1, newIx2, newRs)
  where
    r1 = rs `SQ.index` ix1
    r2 = rs `SQ.index` ix2
    newRs = rs SQ.>< SQ.fromList (digitToInt <$> show (r1 + r2))
    newIx1 = (ix1 + 1 + r1) `mod` length newRs
    newIx2 = (ix2 + 1 + r2) `mod` length newRs

tenAfter :: Int -> ElfState -> [Int]
tenAfter n s@(_, _, rs)
  | length rs >= n + 10 = F.toList $ SQ.take 10 $ SQ.drop n $ rs
  | otherwise = tenAfter n (step s)

part1 :: String
part1 = intToDigit <$> (tenAfter 598701 mkElfState)

stepUntil :: ElfState -> Int
stepUntil s@(_, _, rs)
  | SQ.take 6 (SQ.drop (length rs - 6) rs) == target = length rs - 6
  | SQ.take 6 (SQ.drop (length rs - 7) rs) == target = length rs - 7
  | otherwise = stepUntil (step s)
  where
    target = SQ.fromList [5, 9, 8, 7, 0, 1]

part2 :: Int
part2 = stepUntil mkElfState
