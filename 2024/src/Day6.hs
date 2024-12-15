module Day6 (part1, part2) where

walk :: Grid Char -> Ѓ (Set (ℤ₆₄ × ℤ₆₄)) (?)
walk g = go (∅) def =<< (gridFindOne '^' g)
  where
    go seen d c
      | (c, d) ∈ seen = Nothing
      | c ∉ g = Just $ setMap fst seen
      | g |? move d 1 c == Just '#' = go seen (turnCW d) c
      | otherwise = go ((c, d) |-> seen) d (move d 1 c)

part1 :: Ѓ ℤ (?)
part1 = size <$> walk $(grid 6)

part2 :: Ѓ ℤ (?)
part2 =
  let g = $(grid 6)
      walk' = (((as @ℤ . isNothing . walk . (g ||.) . (,'#')) >>>) . (+))
   in foldl' walk' (-1) <$> (unSet <$> walk g)
