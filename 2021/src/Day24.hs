module Day24 (part1, part2) where

import Data.Char (intToDigit)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.Read (decimal)
import Helper.Util (readOne)

-- Taken from the input; what Z is divided by, the first and second constants in each block
blocks :: [(Int, Int, Int)]
blocks =
  [ (1, 13, 10),
    (1, 11, 16),
    (1, 11, 0),
    (1, 10, 13),
    (26, -14, 7),
    (26, -4, 11),
    (1, 11, 11),
    (26, -3, 10),
    (1, 12, 16),
    (26, -12, 8),
    (1, 13, 15),
    (26, -12, 2),
    (26, -15, 5),
    (26, -12, 10)
  ]

solve :: [Int] -> Int
solve digitRange =
  readOne decimal . T.pack . fmap intToDigit . reverse $
    (go (M.singleton 0 []) blocks) M.! 0
  where
    go :: Map Int [Int] -> [(Int, Int, Int)] -> Map Int [Int]
    go m [] = m
    go m ((d, a, b) : blocks) =
      let m' =
            M.fromList
              [ (z', w : v)
                | w <- digitRange,
                  (z, v) <- M.toList m,
                  let z' = if (z `mod` 26) + a == w then z `div` d else (z `div` d) * 26 + (w + b),
                  z' <= 456976
              ]
       in go m' blocks

part1 :: Int
part1 = solve [1 .. 9]

part2 :: Int
part2 = solve [9, 8 .. 1]
