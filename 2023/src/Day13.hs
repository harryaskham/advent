module Day13 (part1, part2) where

reflects :: Grid DotHash -> [Either Int Int]
reflects g' =
  [ f i
    | (f, g) <- [(Left, g'), (Right, r270 (variants g'))],
      let (maxX, _) = maxXY g,
      i <- [1 .. maxX],
      let w = min i (maxX - i + 1),
      g
        & partitionWithKey (\k _ -> k < (i, 0))
        & second (mapKeys (first (subtract i)))
        & bimap (cropX (i - w) i) (cropX 0 w)
        & sortT2On mapSize
        & second (v0 . variants)
        & uncurry (==)
  ]

reflectsSmudged :: Grid DotHash -> [Either Int Int]
reflectsSmudged g =
  filter
    (∉ mkSet (reflects g))
    (nub $ reflects =<< [adjust (bool Dot Hash . (== Dot)) c g | c <- keys g])

solve :: (Grid DotHash -> [Either Int Int]) -> [Grid DotHash] -> Int
solve f = concatMap f >>> partitionEithers >>> both sum >>> second (* 100) >>> uncurry (+)

part1 :: Int
part1 = solve reflects $(grids input 13)

part2 :: Int
part2 = solve reflectsSmudged $(grids input 13)