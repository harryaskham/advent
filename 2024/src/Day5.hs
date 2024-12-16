module Day5 (part1, part2) where

(rules, orders) :: (Map ℤ [ℤ], [[ℤ]]) =
  $(aoc 5)
    |- ( (,)
           <$> (mapcat "|" number number <* eol)
           <*> (many1 ((number @ℤ `sepBy1` char ',') <* eol))
       )

mkCompare :: Map ℤ [ℤ] -> ℤ -> ℤ -> Ordering
mkCompare lt a b
  | a == b = EQ
  | b ∈ (lt |? a ? [b]) = LT
  | otherwise = GT

solve f = sum (zipWith (\a b -> bool 0 (middle a) (a == b)) orders (sortBy (mkCompare rules) <$> orders))

part1 = solve bool

part2 = solve (\a b c -> bool b a c)
