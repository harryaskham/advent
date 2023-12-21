module Day21 (part1, part2) where

walk :: Int -> Grid Char -> Int
walk n g' = size (go n (mkSet [start]))
  where
    start = gridFindOne 'S' g'
    g = g' |. (start, '.')
    go 0 cs = cs
    go n cs = go (n - 1) (mkSet [c' | c <- unSet cs, c' <- neighborsNoDiags c, g |? c' == Just '.'])

walk' :: Int -> Grid Char -> Int
walk' n' g' =
  -- sum . fmap size . filter ((n' - 1) ∈) . fmap (setMap fst . snd) .
  let counts = unMap $ go (mkSeq [(0, start)]) initCounts -- initCounts
   in length $ nub [c | (c', ncs) <- counts, (n, c) <- unSet ncs, n == n']
  where
    (w, h) = both (+ 1) (maxXY g')
    initCounts = mkMap [((x, y), mkSet []) | x <- [0 .. w - 1], y <- [0 .. h - 1]]
    start = gridFindOne 'S' g'
    g = g' |. (start, '.')
    cMod c l
      | c == 0 = 0
      | c < 0 = l - 1 + (c `mod` l)
      | c < l = c
      | otherwise = c `mod` l
    -- local (x, y) = (x `cMod` w, y `cMod` h)
    local (x, y) = (x `mod` w, y `mod` h)
    get g c =
      -- traceShow (c, local c, w, h) $
      g |! local c
    -- go ((n, c) :<| q) counts localCounts
    go ((n, c) :<| q) localCounts
      | n > n' = localCounts
      | otherwise =
          traceShow (c, n) $
            go
              (q >< mkSeq [(n + 1, c') | c' <- neighborsNoDiags c, g `get` c' /= '#'])
              -- (counts |~ (c, (<-| n)))
              (localCounts |~ (local c, (<-| (n, c))))

part1 :: Int
part1 = walk 64 $(grid input 21)

-- factorize 26501365 = 5x11x481843
-- part2 = walk' 26501365 $(grid input 21)
part2 =
  [ walk' 6 $(grid exampleInput 21),
    walk' 10 $(grid exampleInput 21)
    -- walk' 50 $(grid exampleInput 21),
    -- walk' 100 $(grid exampleInput 21),
    -- walk' 500 $(grid exampleInput 21),
    -- walk' 1000 $(grid exampleInput 21),
    -- walk' 5000 $(grid exampleInput 21)
  ]
