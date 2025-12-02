module Day2 (part1, part2) where

invalid :: Integer -> Integer -> Bool
invalid i s =
  let n = (floor (logBase 10 (fromInteger i)) + 1)
      t = n `div` s
      e = 10 ^ t
      r = i `div` (10 ^ (n - t))
      o = sum [r * 10 ^ j | j <- [0, t .. n - 1]]
   in if t == 0 || t >= n || n `mod` t /= 0
        then False
        -- else traceShow (i, s, t, n, e, r, o) $ i == o
        else i == o

oneRange :: [Integer] -> Parser (Σ Integer)
oneRange r = do
  (a, b) <- (,) <$> (number <* char '-') <*> number
  return (mconcat [Σ i | i <- [a .. b], any (invalid i) r])

part1 :: Σ Integer
part1 = $(aoc 2) |-<> oneRange [2] `sepBy` char ','

part2 :: Σ Integer
part2 = $(aoc 2) |-<> oneRange [1 .. 10] `sepBy` char ','
