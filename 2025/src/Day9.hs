module Day9 (part1, part2) where

(part1, part2) :: ℤ × ℤ =
  let (out, rects) = outside ∘ loopPairs &&& triPairs $ (((⋮) @([ℤ ⹉ 2] ≠ []) $(aoc 9)) ⊏)
   in ((!>) ∘ Ȟ ∘ (🎝) ∘ (ds² <$@>) ∘ (rects |-?->)) <:> ((⊨), not ∘ or ∘ (out <&>) ∘ (□?□))
