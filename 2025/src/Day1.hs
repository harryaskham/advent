module Day1 (part1, part2) where

((part1, part2) :: (ℤ', ℤ')) =
  ($(aoc 1) |-.. ((*) <$> (((string "L") $> (-1)) <|> ((string "R") $> 1)) <*> number))
    ⥢ (⇲ (>>= ((replicate $@) . (abs &&& sgn))))
    & both ((50 :) >>> scanl1 ((`mod` 100) .<. (+)) >>> counts >>> (|! 0))
