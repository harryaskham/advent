module Day13 (part1, part2) where

descend :: ℝ -> ℝ² -> [ℝ] -> Σ ℤ
descend last (a, b) claw@[ax, ay, bx, by, px, py]
  | l ≡ last = 0
  | p (as @ℤ² (a, b)) ≡ as @ℤ² (px, py) = as @(Σ ℤ) (3 ⋅ a) + as @(Σ ℤ) b
  | otherwise = descend l (a - (da / ax), b - (db / bx)) claw
  where
    p (a, b) = (a ⋅ as @ℤ ax + b ⋅ as @ℤ bx, a ⋅ as @ℤ ay + b ⋅ as @ℤ by)
    (dx, dy) = (\a b -> a ⋅ ax + b ⋅ bx - px, \a b -> a ⋅ ay + b ⋅ by - py)
    loss (a, b) = (dx a b) ** 2 + (dy a b) ** 2 + 3 * a + b
    grad (a, b) =
      ( 2 ⋅ (ax ⋅ (dx a b) + ay ⋅ (dy a b)) + 3,
        2 ⋅ (bx ⋅ (dx a b) + by ⋅ (dy a b)) + 1
      )
    l = loss (a, b)
    (da, db) = both (* 0.001) (grad (a, b))

claws :: [[ℝ]]
claws = $(aoc 13) |-..<> numbers & chunksOf 6

part1 :: Σ ℤ
part1 = (claws <&> descend 0 (0, 0) <>!)

part2 :: Σ ℤ
part2 =
  ( claws
      & (mapped ∘ traversed ∘ indices (∈ [4 :: ℤ₆₄, 5]))
      +~ 10000000000000
      <&> descend 0 (0, 0)
      <>!
  )
