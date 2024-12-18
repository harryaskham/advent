module Day4 (part1, part2) where

occurrences :: forall n. n -*=| Text |=*-> Σ ℤ
occurrences = variadicat @n @Text $ \kernel ->
  cells
    |=< convolveWith (Σ . as @ℤ .<. x_x)
    <$> rotations (readGrid kernel)
    <*> pure ($(grid 4) :: HashGrid' (ℤ₆₄, ℤ₆₄) Char)

part1 :: Σ ℤ
part1 =
  occurrences @2
    [txt|XMAS|]
    [txt|X___
         _M__
         __A_
         ___S|]

part2 :: Σ ℤ
part2 =
  occurrences @1
    [txt|M_S
         _A_
         M_S|]
