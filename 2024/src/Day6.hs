module Day6 (part1, part2) where

walk :: Grid Char -> Ѓ (Set Coord2) (?)
walk g = go (∅) def =<< (gridFindOne '^' g)
  where
    go seen d c
      | (c, d) ∈ seen = Nothing
      | c ∉ g = Just $ setMap fst seen
      | g |? move d 1 c == Just '#' = go seen (turnCW d) c
      | otherwise = go ((c, d) |-> seen) d (move d 1 c)

part1 :: Maybe Int
part1 = size <$> walk (readGrid $(input 6))

part2 :: Ѓ Int (?)
part2 =
  let g = readGrid $(input 6)
   in foldl'
        (((bool 0 1 . isNothing . walk . (g ||.) . (,'#')) >>>) . (+))
        (-1)
        <$> (unSet <$> walk g)
