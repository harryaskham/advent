module Day8 (part1, part2) where

antinodes :: ℤ -> ℤ -> Map Char [ℤ × ℤ] -> [ℤ × ℤ]
antinodes l n =
  nub ∘ (>>= ((>>= (\(a, b) -> bool [b + (i, i) ⋅ (b - a) | i <- [l .. n]] [] (a ≡ b))) ∘ uncurry cartesian ∘ dup)) ∘ values ∘ (|/ '.')

solve l n =
  readGrid @Grid' @(ℤ × ℤ) @Char $(aoc 8)
    & dup
    & ((∋) &<$>& (antinodes l n ∘ co @(Grid' (ℤ × ℤ) Char) @(Map Char [ℤ × ℤ])))
    & filter (≡ True)
    & size

part1 :: ℤ
part1 = solve 1 1

-- 946 too low
part2 :: ℤ
part2 = stabilize (solve 0) [100 ..]
