module Day17 (part1, part2) where

minimizeHeatLoss :: (Int -> Bool) -> (Int -> Bool) -> Grid Int -> Int
minimizeHeatLoss turnP noTurnP g =
  let target = maxXY g
      go cache q
        | c == target && turnP l = cost + (g |! c) - (g |! (0, 0))
        | k ∈ cache && cache |! k <= cost = go cache rest
        | otherwise = go cache' (foldl' (|.) rest next)
        where
          ((_, (cost, k@(c, l, d))), rest) = (q <!)
          cache' = if k ∈ cache then cache |~ (k, min cost) else cache |. (k, cost)
          next =
            [ (cost + (g |! c') + manhattan c' target, (cost + g |! c, (c', l', d')))
              | (d', p, l') <- (d, noTurnP l, l + 1) : ((,turnP l,1) <$> [turnCW d, turnCCW d]),
                let c' = move d' 1 c,
                p && c' ∈ g
            ]
   in go (mkMap []) (mkMinQ [(0, (0, ((0, 0), 0, d))) | d <- [DirDown, DirRight]])

part1 :: Int
part1 = $(grid input 17) & minimizeHeatLoss (const True) (< 3)

part2 :: Int
part2 = $(grid input 17) & minimizeHeatLoss (>= 4) (< 10)
