module Day13 where

import Control.Lens

opt :: [ℤ] -> ℤ
opt [ax', ay', bx', by', px', py'] = go 0 (0, 0)
  where
    [ax, ay, bx, by, px, py] = fromIntegral <$> [ax', ay', bx', by', px', py']
    p (a, b) = (a ⋅ ax' + b ⋅ bx', a ⋅ ay' + b ⋅ by')
    loss (a, b) = (a ⋅ ax + b ⋅ bx - px) ** 2 + (a ⋅ ay + b ⋅ by - py) ** 2 + 3 * a + b
    grad (a, b) =
      ( 2 ⋅ (ax ⋅ (a ⋅ ax + b ⋅ bx - px) + ay ⋅ (a ⋅ ay + b ⋅ by - py)) + 3,
        2 ⋅ (bx ⋅ (a ⋅ ax + b ⋅ bx - px) + by ⋅ (a ⋅ ay + b ⋅ by - py)) + 1
      )
    go last (a, b)
      | l ≡ last = 0
      | p (a', b') ≡ (px', py') = 3 ⋅ a' + b'
      | otherwise = go l (a - (da / ax), b - (db / bx))
      where
        l = loss (a, b)
        lr = 0.001
        (da', db') = grad (a, b)
        (da, db) = (lr ⋅ da', lr ⋅ db')
        (a', b') = (round a, round b)

claws :: [[ℤ]]
claws = chunksOf 6 $ $(aoc 13) |-..<> numbers @ℤ

part1 :: ℤ
part1 = sum (opt 0 <$> claws)

part2 :: ℤ
part2 = sum (opt (claws & indices (∈ [4, 5]) %~ (+ 10000000000000)))
