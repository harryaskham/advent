module Day1 (part1, part2) where

import Data.Text (replace)

turns :: [Int]
turns =
  ( $(input (1 :: Int))
      & replace "L" "-"
      & replace "R" ""
      & unpack
  )
    |-.. number

turn x r = (x + r) `mod` 100

part1 :: Int
part1 = (turns & scanl' turn 50 & counts) |! 0

part2 :: Int
part2 =
  let ts = mconcat ((\x -> replicate (abs x) (signum x)) <$> turns)
   in (ts & scanl' turn 50 & counts) |! 0
