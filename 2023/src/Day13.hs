module Day13 (part1, part2) where

reflects :: Grid DotHash -> Set (Either ℤ' ℤ')
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

reflectsSmudged :: Grid DotHash -> Set (Either ℤ' ℤ')
reflectsSmudged g =
  setFilter
    (∉ reflects g)
    (foldl1 (∪) $ reflects <$> [g |~ (c, bool Dot Hash . (== Dot)) | c <- coords g])

solve :: (Grid DotHash -> [Either ℤ' ℤ']) -> [Grid DotHash] -> ℤ'
solve f = concatMap f >>> partitionEithers >>> both sum >>> second (* 100) >>> uncurry (+)

part1 :: ℤ'
part1 = solve (co . reflects) $(grids input 13)

part2 :: ℤ'
part2 = solve (co . reflectsSmudged) $(grids input 13)