module TwentySeventeen.Day10 where

import Data.Bits (Bits (xor))
import Data.Char (ord)
import qualified Data.Foldable as F
import Data.List.Split (chunksOf, splitOn)
import qualified Data.Sequence as SQ
import Text.Printf (printf)

input :: String
input = "106,118,236,1,130,0,235,254,59,205,2,87,129,25,255,118"

step :: SQ.Seq Int -> Int -> [Int] -> Int -> SQ.Seq Int
step ring _ [] start = let (x1, x2) = SQ.splitAt start ring in x2 SQ.>< x1
step ring skip (i : is) start = step (b SQ.>< a) (skip + 1) is newStart
  where
    (xs, rest') = SQ.splitAt i ring
    reversed = (SQ.reverse xs) SQ.>< rest'
    toMove = (skip + i) `mod` length ring
    (a, b) = SQ.splitAt toMove reversed
    newStart = (start - toMove) `mod` length ring

part1 :: Int
part1 = product $ SQ.take 2 ring
  where
    is = read <$> splitOn "," input
    ring = step (SQ.fromList [0 .. 255]) 0 is 0

part2 :: String
part2 = concat $ printf "%02x" . foldl1 xor <$> chunks
  where
    is = concat $ replicate 64 ((ord <$> input) ++ [17, 31, 73, 47, 23])
    ring = step (SQ.fromList [0 .. 255]) 0 is 0
    chunks = chunksOf 16 $ F.toList ring
