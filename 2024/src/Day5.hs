module Day5 (part1, part2) where

parts :: (Σ ℤ, Σ ℤ)
parts =
  $(aoc 5) |-<> do
    order :: Map ℤ [ℤ] <- mapcat "|" number number <* eol
    let cmp = bool GT LT .<. (⇄ ((? True) .<. ((. (order |?)) . (<$>) . (∈))))
    linesOf (csvOf (number @ℤ))
      <&> (⤊ (\(a, b) -> bool (Σ a, Σ 0) (Σ 0, Σ b)))
      . (⥢ (bimap (uncurry zip . both (<&> middle)) (⤊ (==))))
      . (⥢ (second (<&> sortBy cmp)))

part1 :: Σ ℤ
part1 = fst parts

part2 :: Σ ℤ
part2 = snd parts
