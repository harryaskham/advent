module Day24 (part1, part2) where

import Data.Char (intToDigit)
import Data.List ((!!))
import Data.List.Extra (chunksOf)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.Read (decimal)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Helper.TH (input)
import Helper.Util (number, parseWith, readOne, toTuple3)
import Text.ParserCombinators.Parsec (anyChar, count)

blocks :: Vector (Int, Int, Int)
blocks =
  $(input 24)
    & T.lines
    & fmap T.unpack
    & chunksOf 18
    & fmap (\ls -> toTuple3 (parseWith (count 6 anyChar *> number) . (ls !!) <$> [4, 5, 15]))
    & V.fromList

solve :: ([Int] -> [Int] -> [Int]) -> Int
solve minMax = readOne decimal . T.pack $ (intToDigit <$> go (M.singleton 0 []))
  where
    go izws
      | (length <$> M.lookup 0 izws) == Just (length blocks) = izws M.! 0
      | otherwise =
        traceShow (M.size izws) $
          go $
            M.fromListWith
              minMax
              [ (z', w : ws)
                | w <- [1 .. 9 :: Int],
                  (z, ws) <- M.toList izws,
                  let i = length ws,
                  let (d, a, b) = blocks V.! i,
                  let z' = if (z `mod` 26) + a == w then z `div` d else (z `div` d) * 26 + (w + b)
              ]

part1 :: Int
part1 = solve max

part2 :: Int
part2 = solve min
