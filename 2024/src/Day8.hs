module Day8 (part1, part2) where

antinodes :: ℤ -> ℤ -> Map Char [ℤ × ℤ] -> [ℤ × ℤ]
antinodes l u =
  let nodes (a, b) = (a ≡ b) ??? [b + (i, i) ⋅ (b - a) | i <- [l .. u]] $ []
   in nub ∘ (>>= (>>= nodes) ∘ join cartesian) ∘ values ∘ (|/ '.')

step :: ℤ -> ℤ -> Grid' (ℤ × ℤ) Char -> Σ ℤ
step l u = ((as @(Σ ℤ) .<. (∋)) &<$>& (antinodes l u ∘ co)) <>∘ dup

part1 :: Σ ℤ
part1 = step 1 1 $ $(grid 8)

part2 :: Σ ℤ
part2 = step 0 ∘ (max $@) ∘ maxXY $$@ $(grid 8)
