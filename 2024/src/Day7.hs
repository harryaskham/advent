module Day7 (part1, part2) where

with :: [ℤ -> ℤ -> ℤ] -> Parser (Σ ℤ)
with ops =
  let f (x : xs) = (ops >>=) . ((f xs . ($ x)) .<. (&))
      f [] = pure
      g t = bool (Σ 0) (Σ t) . (t ∈) . ((⇄ f) 0)
   in g <$@> snoc <$> numbers

part1 :: Σ ℤ
part1 = $(aoc 7) |-<..?!> with [(+), (*)]

part2 :: Σ ℤ
part2 =
  let a || b = number -| show a <> show b
   in $(aoc 7) |-<..?!> with [(+), (*), (||)]
