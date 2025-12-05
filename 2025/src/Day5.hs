module Day5 (part1, part2) where

(ranges, ids) :: ([ℤ²], [ℤ]) = $(aoc 5) |- ((,) <$> (sort <$> many (natRange <* eol)) <*> naturals)

part1 :: Σ ℤ
part1 = ([Σ 1 | i <- ids, or [inRange r i | r <- ranges]] <>!)

merge :: (ℤ, ℤ²) -> ℤ² -> (ℤ, ℤ²)
merge (n, (a, b)) (c, d)
  | d < b = (n, (a, b))
  | c <= b = (n + rlen Excl (b, d), (a, d))
  | otherwise = (n + rlen Incl (c, d), (c, d))

part2 :: ℤ
part2 =
  let (r : rs) = ranges
      (n :: ℤ, _ :: ℤ²) = ((Ł merge (rlen Incl r, r) rs) !>)
   in n
