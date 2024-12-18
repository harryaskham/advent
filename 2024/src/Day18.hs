module Day18 (part1, part2) where

grids :: ℤ² -> Vector ℤ² -> Vector (G ℤ² Char)
grids (w, h) =
  mk
    ∘ (scanl' ((∘ (,'#')) ∘ (|.)))
      (defGrid [(x, y) | x <- [0 .. w], y <- [0 .. h]])
    ∘ un

path :: ℤ² -> Vector (G ℤ² Char) -> [ℤ²]
path dims gs = go (mkQ₁ h ((0, 0), 0, [])) (mkSet [])
  where
    g t = gs !! (min (t + 1) (size gs - 1))
    h (c, t, _) = t + manhattan c dims
    go NullQ _ = []
    go ((c, t, p) :<!! q) seen
      | c ≡ dims = p
      | not (cellEmpty c (g t)) ∨ c ∈ seen = go q seen
      | otherwise =
          let ns = neighs @4 @[] c (g t)
           in go (foldr (qInsert h ∘ (,t + 1,c : p)) q ns) (c |-> seen)

(part1, part2) :: (ℤ, String) =
  let (dims, bytes) = ((70, 70), mk $ $(aoc 18) |- tuples @2 (numbers @ℤ))
      gs = grids dims bytes
      paths n = path dims (pure (gs !! (n + 1)))
   in ( size (paths 1024),
        ((<>) $@)
          ∘ first (<> ",")
          ∘ both show
          $ (bytes !! ((searchFromTo (null ∘ paths) 1025 (size bytes - 1)) ? -1))
      )
