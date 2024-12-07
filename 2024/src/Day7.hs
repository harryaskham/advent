module Day7 (part1, part2) where

sat :: [ℤ -> ℤ -> ℤ] -> Parser (Σ ℤ)
sat ops = do
  t <- (number @ℤ) <* string ": "
  xs <- many1 (wordOf (number @ℤ)) <* eof
  let f s (x : xs) = mconcat [f (s `op` x) xs | op <- ops]
      f s [] = [s]
  let ts = f 0 xs
  return $ if t `elem` ts then Σ t else Σ 0

part1 :: Σ ℤ
part1 = $(aoc 7) |-<..?!> sat [(+), (*)]

part2 :: Σ ℤ
part2 =
  let a || b = number -| show a <> show b
   in $(aoc 7) |-<..?!> sat [(+), (*), (||)]
