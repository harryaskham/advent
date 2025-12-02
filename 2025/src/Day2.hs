module Day2 (part1, part2) where

invalid :: ℤ -> ℤ -> ℤ -> Bool
invalid i l s =
  let t = l `div` s
      r = i `div` (10 ^ (l - t))
      o = sum [r * 10 ^ j | j <- [0, t .. l - 1]]
   in t * s == l && i == o

aRange :: [ℤ] -> Parser (Σ ℤ)
aRange s = do
  (a, b) <- (,) <$> (number <* char '-') <*> number
  let o = [Σ i | i <- [a .. b], let l = floor (logBase 10 (fromInteger i)) + 1, any (invalid i l) s]
  pure (o <>!)

part1 :: Σ ℤ
part1 = $(aoc 2) |-<> csvOf (aRange [2])

part2 :: Σ ℤ
part2 = $(aoc 2) |-<> csvOf (aRange [1 .. 10])
