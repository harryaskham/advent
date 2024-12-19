module Day19 (part1, part2) where

(part1, part2) :: (Σ ℤ, Σ ℤ) =
  let go ts "" = pure (Σ 1)
      go ts p = go ts .=<<. (stripPrefix <$> splitOn ", " ts <*> [p] <>?)
   in ((⥢ ((±) ⇱)) ∘ run ∘<∘ go &<$> snoc (flines $(aoc 19)) <>!)
