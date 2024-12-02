module Day2 (part1, part2) where

safe :: 𝔹 -> [ℤ₆₄] -> Parser (Σ ℤ₆₄)
safe skip ls =
  trying
    [ safe skip . (: ls) =<< level ls,
      guard skip >> try (level [] >> safe False ls),
      succeed (Σ 1)
    ]
  where
    level [] = wordOf $ number @ℤ₆₄
    level ls =
      level [] >>= \l -> do
        let (d : ds) = diffs (l : ls)
        l <$ guard (all ((`elem` [1, 2, 3]) . (* sgn d)) (d : ds))

part1 :: ℤ₆₄
part1 = $(input 2) |-<> safe False []

part2 :: ℤ₆₄
part2 = $(input 2) |-<> safe True []
