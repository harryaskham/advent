module Day5 (part1, part2) where

(ranges, ids) :: [ℤ |-| ℤ] × [ℤ] = $(aoc 5) |- ((,) <$> many (p @(ℕ |-| ℕ) <* eol) <*> nats)

part1 :: ℤ = ((ids |-?-> (or ∘ (<$> ranges) ∘ (∈))) |.|)

part2 :: ℤ = (((Ŀ (((<<*>>) ∘ (max <:>)) &.& ((⇱) ∘ (-!) ∘ (|.|))) (ranges 🎜)) !>) |.|)
