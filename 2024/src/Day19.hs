module Day19 (part1, part2) where

(part1, part2) :: (Σ ℤ, Σ ℤ) =
  (((⥢ (first signum)) ∘ run ∘ go <$> ps) <>!)
  where
    (ts' : _ : ps) = lines $(aoc 19)
    ts = splitOn ", " ts'
    go :: Text .->. Σ ℤ
    go "" = return (Σ 1)
    go p =
      case catMaybes (stripPrefix <$> ts <*> pure p) of
        [] -> return (Σ 0)
        ps -> ([memo go p | p <- ps] <>!)
