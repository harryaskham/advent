module Day19 (part1, part2) where

(part1, part2) :: (Σ ℤ, Σ ℤ) =
  let (ts : _ : ps) = lines $(aoc 19)
      go "" = return (Σ 1)
      go p = go .=<<. (stripPrefix <$> splitOn ", " ts <*> pure p <>?)
   in ((⥢ (first signum)) ∘ run ∘ (go :: Text .->. Σ ℤ) <$> ps <>!)
