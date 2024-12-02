module Day2 (part1, part2) where

safe :: 𝔹 -> [ℤ₆₄] -> Parser (Σ ℤ₆₄)
safe skip ls =
  trying
    [ do
        ls' <- wordOf number <&> (: ls)
        let (d : ds) = diffs ls'
        guard (null ls || all ((`elem` [1, 2, 3]) . (* sgn d)) (d : ds))
        safe skip ls',
      guard skip >> anyWord >> safe False ls,
      succeed (Σ 1)
    ]

part1 :: ℤ₆₄
part1 = $(input 2) |-<> safe False []

part2 :: ℤ₆₄
part2 = $(input 2) |-<> safe True []
