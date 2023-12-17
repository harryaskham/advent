module Day17 (part1, part2) where

minimizeHeatLoss :: (Int -> Bool) -> (Int -> Bool) -> Grid Int -> Int
minimizeHeatLoss turnP noTurnP g = go (mkMap []) (mkMinQ [(0, ((0, 0), 0, 0, d)) | d <- [DirDown, DirRight]])
  where
    target = maxXY g
    go cache q
      | c == target && turnP l = cost + (g |! c) - (g |! (0, 0))
      | key ∈ cache && cache |! key <= cost = go cache rest
      | otherwise = go cache' q'
      where
        ((_, (c, cost, l, d)), rest) = (q <!)
        key = (c, d, l)
        cache' = cache |. (key, fromMaybe cost (cache |? key)) |~ (key, min cost)
        next =
          [ (c', cost + g |! c, l', d')
            | (d', p, l') <- (d, noTurnP l, l + 1) : ((,turnP l,1) <$> [turnCW d, turnCCW d]),
              p,
              let c' = move d' 1 c,
              c' ∈ g
          ]
        q' = foldl' (\q s@(c, cost, _, _) -> q |. (cost + (g |! c) + manhattan c target, s)) rest next

part1 :: Int
part1 = $(grid input 17) & minimizeHeatLoss (const True) (< 3)

part2 :: Int
part2 = $(grid input 17) & minimizeHeatLoss (>= 4) (< 10)
