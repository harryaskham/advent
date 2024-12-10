module Day10 (part1, part2) where

trails :: 𝔹 -> ℤ² ℕ₁₀ -> (ℤ × ℤ) -> [ℤ × ℤ]
trails skip g c =
  let uphill n = g |! n ≡ g |! c + 1
      go (c :<| cs) seen nines
        | skip ∧ c ∈ seen = go cs seen nines
        | g |! c ≡ 9 = go cs (c |-> seen) (c : nines)
        | otherwise = go (cs >< (neighbors @4 c g |-?-> uphill)) (c |-> seen) nines
      go _ _ nines = nines
   in go (mk₁ c) ø ø

hike :: ([ℤ × ℤ] -> [ℤ × ℤ]) -> 𝔹 -> Σ ℤ
hike f skip = (readGrid $(aoc 10) ⥢ (((Σ ∘ size ∘ f) ∘<∘ trails skip) &<$>& (|?> (0 :: ℕ₁₀))) <>!)

part1 :: Σ ℤ
part1 = hike nub True

part2 :: Σ ℤ
part2 = hike id False
