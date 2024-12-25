module Day25 (part1, part2) where

part1 :: Σ ℤ
part1 =
  let (gs :: [".#" ▦ ℤ²]) = gridsT @Text $(aoc 25)
      f g g' = Σ ∘ as @ℤ $ (dup (#"#" □)) ∉ zip (cells g) (cells g')
   in ((f <$@> triPairs gs) <>!)

part2 :: Text
part2 = "Merry Christmas!"
