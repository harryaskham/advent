module Day13 where

sgd :: [ℤ] -> Σ ℤ
sgd [ax', ay', bx', by', px', py'] = f 0 (0, 0)
  where
    [ax, ay, bx, by, px, py] = fromIntegral <$> [ax', ay', bx', by', px', py']
    p (a, b) = (a ⋅ ax' + b ⋅ bx', a ⋅ ay' + b ⋅ by')
    loss (a, b) = (a ⋅ ax + b ⋅ bx - px) ** 2 + (a ⋅ ay + b ⋅ by - py) ** 2 + 3 * a + b
    grad (a, b) =
      ( 2 ⋅ (ax ⋅ (a ⋅ ax + b ⋅ bx - px) + ay ⋅ (a ⋅ ay + b ⋅ by - py)) + 3,
        2 ⋅ (bx ⋅ (a ⋅ ax + b ⋅ bx - px) + by ⋅ (a ⋅ ay + b ⋅ by - py)) + 1
      )
    f last (a, b)
      | l ≡ last = 0
      | p (a', b') ≡ (px', py') = Σ (3 ⋅ a' + b')
      | otherwise = f l (a - (da / ax), b - (db / bx))
      where
        l = loss (a, b)
        lr = 0.001
        (da', db') = grad (a, b)
        (da, db) = (lr ⋅ da', lr ⋅ db')
        (a', b') = (round a, round b)

claws :: [[ℤ]]
claws = chunksOf 6 $ $(aoc 13) |-..<> numbers @ℤ

part1 :: Σ ℤ
part1 = (claws <&> sgd <>!)

part2 :: Σ ℤ
part2 = ((claws & mapped ∘ traversed ∘ indices (∈ [4 :: ℤ₆₄, 5]) %~ (+ 10000000000000)) <&> sgd <>!)
