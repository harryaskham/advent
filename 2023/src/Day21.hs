module Day21 (part1, part2) where

walk :: ℤ' -> Grid Char -> ℤ'
walk n g' = size (go n (mkSet [start]))
  where
    start = gridFindOne 'S' g'
    g = g' |. (start, '.')
    go 0 cs = cs
    go n cs = go (n - 1) (setFilter (\c -> g |? c == Just '.') . setConcatMap (co . neighborsNoDiags) $ cs)

part1 :: ℤ'
part1 = walk 64 $(grid input 21)

part2 :: ℤ'
part2 = do
  let g = $(grid input 21) :: Grid Char
      cs = mkSet (gridFind 'S' g)
      spaces = cs ∪ mkSet (gridFind '.' g)
      go n cs
        | n == 393 = []
        | n `mod` 131 == 65 = size cs : next
        | otherwise = next
        where
          next =
            go
              (n + 1)
              ( mkSet
                  [ (x, y)
                    | c <- (cs ->>),
                      (x, y) <- neighborsNoDiags c,
                      (mod x 131, mod y 131) ∈ spaces
                  ]
              )
      poly n = let [a, b, c] = go 0 cs in a + n * (b - a + (n - 1) * (c + a - 2 * b) `div` 2)
   in poly (26501365 `div` 131)
