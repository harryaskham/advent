module Day2 (part1, part2) where

safe :: 𝔹 -> [ℤ] -> Parser (Σ ℤ)
safe skip ls =
  trying
    [ succeed (Σ 1),
      guard skip >> anyWord >> safe False ls,
      do
        (lss, ds) <- second diffs . dup . (: ls) <$> wordOf number
        guard (null ls || all (`elem` [1, 2, 3]) ((*) <$> ds <*> (nub (sgn <$> ds))))
        safe skip lss
    ]

part1 :: ℤ
part1 = $(aoc 2) |-<..> safe False []

part2 :: ℤ
part2 = $(aoc 2) |-<..> safe True []
