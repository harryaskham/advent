module Day13 (part1, part2) where

reflects :: Grid DotHash -> Set (Either Int Int)
reflects g' =
  co
    [ f i
      | (f, g) <- [(Left, g'), (Right, r270 (variants g'))],
        let maxX = fst $ maxXY g,
        i <- [1 .. maxX],
        let w = min i (maxX - i + 1),
        g
          & partitionCoords (< (i, 0))
          & bimap (cropX (i - w) i) (cropX 0 w . mapCoords (first (subtract i)))
          & sortT2On (length . coords)
          & second (v0 . variants)
          & uncurry (==)
    ]

reflectsSmudged :: Grid DotHash -> Set (Either Int Int)
reflectsSmudged g =
  setFilter
    (∉ reflects g)
    (foldl1 (∪) $ reflects <$> [g |~ (c, bool Dot Hash . (== Dot)) | c <- coords g])

solve :: (Grid DotHash -> [Either Int Int]) -> [Grid DotHash] -> Int
solve f = concatMap f >>> partitionEithers >>> both sum >>> second (* 100) >>> uncurry (+)

part1 :: Int
part1 = solve (co . reflects) $(grids input 13)

part2 :: Int
part2 = solve (co . reflectsSmudged) $(grids input 13)