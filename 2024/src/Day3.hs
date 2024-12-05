module Day3 (part1, part2) where

muls :: 𝔹 -> 𝔹 -> Parser [Σ [Π ℤ]]
muls dont on =
  let mul True = hom Σ $ cargs #mul (hdup number)
      mul False = mul True $> Σ [Π 0]
   in trying
        [ cfunc_ #do *> muls dont True,
          cfunc_ #don't *> muls dont dont,
          (:) <$> mul on <*> muls dont on,
          anyChar >> muls dont on,
          pure []
        ]

part1 :: ℤ
part1 = $(input 3) |-<.> muls True True

part2 :: ℤ
part2 = $(input 3) |-<.> muls False True
