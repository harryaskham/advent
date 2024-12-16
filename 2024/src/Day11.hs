module Day11 (part1, part2) where

type S = Dℤ (Seq (ℕ₁₀))

mkS :: Seq ℕ₁₀ -> S
mkS s = Dec @ℤ s ⋅ Dec (mk₁ 1)

blink :: Mℤ S .->. Seq S
blink (Mℤ 0 s) = return (mk₁ s)
blink (Mℤ n (Dec (Seq₁ 0))) = blink .$. Mℤ (n - 1) (mkS (mk₁ 1))
blink (Mℤ n (Dec s))
  | even (size s) =
      liftA2 (><) $@ both ((blink .$.) ∘ Mℤ (n - 1) ∘ mkS) (halve s)
  | otherwise = blink .$. Mℤ (n - 1) (Dec s * 2024)

blinks :: ℤ -> ℤ
blinks n =
  $(aoc 11)
    |- (mk <$> wordsOf (Mℤ n ∘ mkS ∘ mk <$> many1 nat₁₀))
    & (size ∘ join ∘ run ∘ traverse blink)

part1 :: ℤ
part1 = blinks 25

part2 :: ℤ
part2 = blinks 75
