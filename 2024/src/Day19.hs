module Day19 (part1, part2) where

(part1, part2) :: (Σ ℤ, Σ ℤ) =
  let (ts : _ : ps) = lines $(aoc 19)
      go "" = pure (Σ 1)
      go p = go .=<<. (stripPrefix <$> splitOn ", " ts <*> [p] <>?)
   in ((⥢ first signum) ∘ run ∘ (go :: Text .->. Σ ℤ) <$> ps <>!)
