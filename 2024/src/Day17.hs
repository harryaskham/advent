module Day17 (part1, part2) where

f :: ℤ -> Seq ℕ₈
f 0 = mkSeq []
f a = f₁ a <| f (a `div` 8)

f₁ :: ℤ -> ℕ₈
f₁ a = let b = a `mod` 8 `xor` 3 in as @ℕ₈ $ b `xor` a `div` 2 ^ b `xor` 5 `mod` 8

allA :: Seq ℕ₈ -> ℤ -> [ℤ]
allA Empty a = [a]
allA (ns :|> n) a = allA ns =<< [a' | a' <- [8 ⋅ a .. 8 ⋅ a + 7], f₁ a' ≡ n]

(part1, part2) :: (String, ℤ) =
  $(aoc 17)
    |- (numbers @ℤ <&> (bimap (fst3 ∘ toTup @3) (mkSeq . fmap (as @ℕ₈)) ∘ splitAt 3))
    & bimap (intercalate "," ∘ fmap show ∘ un ∘ f) (minimum ∘ flip allA 0)
