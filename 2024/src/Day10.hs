module Day10 (part1, part2) where

hike :: ℤ² ℕ₁₀ -> (ℤ × ℤ) -> [Σ ℤ]
hike g = (⊥)

part1 :: Σ ℤ
part1 =
  ( ( (readGrid @Grid' @(ℤ × ℤ) @ℕ₁₀ $(aocx 10))
        & dup
        & (hike &=<<& (|?> (0 :: ℕ₁₀)))
    )
      <>!
  )

part2 :: Text
part2 = "Part 2"
