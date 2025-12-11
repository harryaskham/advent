module Day9 (part1, part2) where

(part1, part2) :: ℤ × ℤ =
  let (out, rects) = (((⋮) @([ℤ ⹉ 2] ≠ []) $(aoc 9)) ⊏) & outside ∘ loopPairs &&& triPairs
   in ((!>) ∘ Ȟ ∘ (🎝) ∘ (ds² <$@>) ∘ (rects |-?->)) <:> ((⊨), not ∘ or ∘ (out <&>) ∘ (□?□))
