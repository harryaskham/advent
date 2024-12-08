module Day8 (part1, part2) where

antinodes :: ℤ -> ℤ -> Map Char [ℤ × ℤ] -> [ℤ × ℤ]
antinodes l u =
  nub ∘ (>>= ((>>= (\(a, b) -> bool [b + (i, i) ⋅ (b - a) | i <- [l .. u]] [] (a ≡ b))) ∘ uncurry cartesian ∘ dup)) ∘ values ∘ (|/ '.')

solve l u g =
  dup g
    & ((∋) &<$>& (antinodes l u ∘ co @(Grid' (ℤ × ℤ) Char) @(Map Char [ℤ × ℤ])))
    & filter (≡ True)
    & size

part1 :: ℤ
part1 =
  readGrid @Grid' @(ℤ × ℤ) @Char $(aoc 8)
    & solve 1 1

-- 946 too low
part2 :: ℤ
part2 =
  readGrid @Grid' @(ℤ × ℤ) @Char $(aoc 8)
    & (\g -> solve 0 (max $@ maxXY g))
