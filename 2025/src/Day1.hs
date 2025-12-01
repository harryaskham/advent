module Day1 (part1, part2) where

turns :: [ℤ']
turns =
  ( $(aoc 1)
      |-.. ((*) <$> (((string "L") $> (-1)) <|> ((string "R") $> 1)) <*> number)
  )

turn :: ℤ' -> ℤ' -> ℤ'
turn x r = (x + r) `mod` 100

part1 :: ℤ'
part1 = (turns & scanl' turn 50 & counts) |! 0

part2 :: ℤ'
part2 =
  let ts = turns >>= (\x -> replicate (abs x) (signum x))
   in (ts & scanl' turn 50 & counts) |! 0
