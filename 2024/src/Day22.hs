module Day22 (part1, part2) where

secret :: ℤ -> ℤ
secret s0 = s3
  where
    prune = (`mod` 16777216)
    s1 = prune $ (s0 ⋅ 64) `xor` s0
    s2 = prune $ (s1 `div` 32) `xor` s1
    s3 = prune $ (s2 ⋅ 2048) `xor` s2

secrets :: ℤ -> ℤ -> NonEmpty ℤ³
secrets n x =
  let (s : ss) =
        scanl'
          ( \(s, s₁, _) _ ->
              let s' = secret s
                  s'₁ = decDigit 1 s'
               in (s', s'₁, s'₁ - s₁)
          )
          (x, decDigit 1 x, ꝏ)
          [1 .. n]
   in s :| ss

prices :: NonEmpty ℤ³ -> ℤ⁴ :|-> ℤ
prices =
  let go ((_, _, a₁) :| b@(_, _, b₁) : c@(_, _, c₁) : d@(_, price, d₁) : xs) =
        ((a₁, b₁, c₁, d₁), price) : go (b :| c : d : xs)
      go _ = []
   in mkMap ∘ reverse ∘ go

part1 :: Σ ℤ
part1 = ($(aoc 22) |- numbers @ℤ <&> Σ ∘ fst3 ∘ last ∘ secrets 2000 <>!)

part2 :: Σ ℤ
part2 =
  let pms = $(aocxn 22 1) |- numbers @ℤ <&> prices ∘ secrets 2000
   in maximum
        [ ([Σ (pm |? seq ? 0) | pm <- pms] <>!)
          | seq <- nub (keys <$> pms <>!)
        ]
