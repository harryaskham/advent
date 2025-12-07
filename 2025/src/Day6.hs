module Day6 (part1, part2) where

(ps, ops) :: [[ℕ]] × [ℕ -> ℕ -> ℕ] = (⊤) ⇱ ($(aoc 6) ⋯)

psᵀ :: [[ℕ]] = ((splitOn "\n" $(aoc 6)) ⊤) & splitBy (all (== ' ')) & ((isDigit <-?-|) <$$>) & ((⋯) <$$>)

(part1, part2) :: (Σ ℕ, Σ ℕ) = both ((<>!) . ((Σ . (!>) . (Ŀ $@)) <$>) . (zip ops)) (ps, psᵀ)
