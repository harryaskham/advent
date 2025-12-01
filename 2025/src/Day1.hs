module Day1 (part1, part2) where

turns :: [ℤ']
turns = $(aoc 1) |-.. ((*) <$> (((string "L") $> (-1)) <|> ((string "R") $> 1)) <*> number)

turn :: ℤ' -> ℤ' -> ℤ'
turn x r = (x + r) `mod` 100

zeros :: [ℤ'] -> ℤ'
zeros = scanl' turn 50 >>> counts >>> (|! 0)

part1 :: ℤ'
part1 = zeros turns

part2 :: ℤ'
part2 = zeros (turns >>= ((replicate $@) . (abs &&& sgn)))
