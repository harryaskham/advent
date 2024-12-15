module Day10 (part1, part2) where

trails :: 𝔹 -> G ℤ² ℕ₁₀ -> ℤ² -> [ℤ²]
trails skip g c =
  let above c n = g |! n ≡ g |! c + 1
      go (c :<| cs) seen nines
        | skip ∧ c ∈ seen = go cs seen nines
        | g |! c ≡ 9 = go cs (c |-> seen) (c : nines)
        | otherwise = go (cs >< (neighs @4 c g |-?-> above c)) (c |-> seen) nines
      go _ _ nines = nines
   in go (mk₁ c) ø ø

hike :: ([ℤ²] -> [ℤ²]) -> 𝔹 -> Σ ℤ
hike f skip = ($(grid 10) ⥢ (((Σ ∘ size ∘ f) ∘<∘ trails skip) &<$>& (|?> (0 :: ℕ₁₀))) <>!)

part1 :: Σ ℤ
part1 = hike nub True

part2 :: Σ ℤ
part2 = hike id False
