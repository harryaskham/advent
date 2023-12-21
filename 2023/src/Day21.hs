module Day21 (part1, part2) where

walk :: Int -> Grid Char -> Int
walk n g' = size (go n (mkSet [start]))
  where
    start = gridFindOne 'S' g'
    g = g' |. (start, '.')
    go 0 cs = cs
    go n cs = go (n - 1) (mkSet [c' | c <- unSet cs, c' <- neighborsNoDiags c, g |? c' == Just '.'])


part1 :: Int
part1 = walk 64 $(grid input 21)

part2 :: Int
part2 = do
  let g = $(grid input 21) :: Grid Char
      cs = mkSet (gridFind 'S' g)
      spaces = cs ∪ (mkSet $ gridFind '.' g)
      go n cs
        | n == 393 = []
        | n `mod` 131 == 65 = size cs : next
        | otherwise = next
        where
          next = go (n + 1) (mkSet [(x, y)
                                    | c <- unSet cs,
                                      (x, y) <- neighborsNoDiags c,
                                      (mod x 131, mod y 131) ∈ spaces])
      poly n = let [a,b,c] = go 0 cs in a + n * (b - a + (n - 1) * (c + a - 2*b) `div` 2)
   in poly (26501365 `div` 131)
