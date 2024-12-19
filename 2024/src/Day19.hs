module Day19 (part1, part2) where

(part1, part2) :: (Σ ℤ, Σ ℤ) =
  let go t p = p ≡ ø ??? go t .=<<. (stripPrefix <$> splitOn ", " t <*> [p] <>?) $ (Σ 1 #)
   in ((⥢ ((±) ⇱)) ∘ run ∘<∘ go &<$> snoc (flines $(aoc 19)) <>!)
