module Day17 (part1, part2) where

runFrom :: ℤ -> Seq ℕ₈ -> ℤ³ -> Seq ℕ₈ -> Seq ℕ₈
runFrom ip out reg@(rA, rB, rC) p =
  case (,) <$> (p !? ip) <*> pure (p !! (ip + 1)) of
    Nothing -> out
    Just (opc, opr) ->
      let (ip', out', reg') =
            case opc of
              0 -> (ip + 2, out, (rA `div` (2 ^ combo opr), rB, rC))
              1 -> (ip + 2, out, (rA, rB `xor` (as @ℤ opr), rC))
              2 -> (ip + 2, out, (rA, (combo opr) `mod` 8, rC))
              3 -> case rA of
                0 -> (ip + 2, out, reg)
                _ -> (as @ℤ opr, out, reg)
              4 -> (ip + 2, out, (rA, rB `xor` rC, rC))
              5 -> (ip + 2, out |> as @ℕ₈ ((combo opr) `mod` 8), reg)
              6 -> (ip + 2, out, (rA, rA `div` (2 ^ combo opr), rC))
              7 -> (ip + 2, out, (rA, rB, (rA `div` (2 ^ combo opr))))
       in runFrom ip' out' reg' p
  where
    combo 4 = rA
    combo 5 = rB
    combo 6 = rC
    combo n = as @ℤ n

f :: ℤ -> Seq ℕ₈
f 0 = mkSeq []
f a = f₁ a <| f (a `div` 8)

f₁ :: ℤ -> ℕ₈
f₁ a =
  as @ℕ₈ $
    ( ( ((a `mod` 8) `xor` 3)
          `xor` (a `div` (2 ^ ((a `mod` 8) `xor` 3)))
      )
        `xor` 5
    )
      `mod` 8

allA :: Seq ℕ₈ -> ℤ -> [ℤ]
allA Empty a = [a]
allA (ns :|> n) a = allA ns =<< [a' | a' <- [8 ⋅ a .. 8 ⋅ a + 7], f₁ a' ≡ n]

(part1, part2) :: (String, ℤ) =
  $(aoc 17)
    |- (numbers @ℤ <&> (bimap (toTup @3) (mkSeq . fmap (as @ℕ₈)) ∘ splitAt 3))
    & (\(reg, p) -> (runFrom 0 ø reg p, minimum (allA p 0)))
    & first (un >>> fmap show >>> intercalate ",")
