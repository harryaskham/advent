module Day5 (part1, part2) where

(ranges, ids) :: ([ℤ²], [ℤ]) =
  $(aoc 5) |- ((,) <$> (sort <$> many (natRange <* eol)) <*> naturals)

part1 :: Σ ℤ
part1 = ([Σ 1 | i <- ids, or [inRange r i | r <- ranges]] <>!)

part2 :: ℤ
part2 =
  let merge (a, b) (c, d)
        | d < b = (a, b)
        | c <= b = (a, d)
        | otherwise = (c - rlen Incl (a, b), d)
   in rlen Incl ((Ŀ merge ranges) !>)
