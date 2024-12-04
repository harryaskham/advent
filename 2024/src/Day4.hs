module Day4 (part1, part2) where

occurrences :: [Text] -> ℤ
occurrences kernels =
  cells
    |=< convolveWith ((Σ . bool (0 :: Integer) 1) .<. wildEq '_')
    <$> (rotations . readGrid =<< kernels)
    <*> [readGrid @HashGrid' $(input 4)]

part1 :: ℤ
part1 =
  occurrences
    [ "XMAS",
      [txt|X___
           _M__
           __A_
           ___S|]
    ]

part2 :: ℤ
part2 =
  occurrences
    [ [txt|M_S
           _A_
           M_S|]
    ]
