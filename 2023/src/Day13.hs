module Day13 (part1, part2) where

reflects :: Grid DotHash -> [Either Int Int]
reflects g =
  let column g =
        [ i
          | let (maxX, _) = maxXY g,
            i <- [1 .. maxX],
            let w = min i (maxX - i + 1)
                crop i j g =
                  let g' = filterWithKey (\(x, _) _ -> x >= i && x < j) g
                      xO = fst $ minXY g'
                   in mapKeys (first (subtract xO)) g',
            let (l, r) = bimap (crop (i - w) i) (crop 0 w) . second (mapKeys (first (subtract i))) $ partitionWithKey (\k _ -> k < (i, 0)) g,
            let (a, b) = second (v0 . variants) . toTuple2 . sortOn mapSize $ [l, r],
            (mkSet . unMap $ a) ⊆ (mkSet . unMap $ b)
        ]
   in (Left <$> column g) <> (Right <$> (column (r90 $ variants g)))

reflectsSmudged :: Grid DotHash -> [Either Int Int]
reflectsSmudged g =
  let smudge = bool Dot Hash . (== Dot)
      previous = uhead (reflects g)
      gs = [insert c (smudge (g |! c)) g | c <- keys g]
   in traceTextLn (pretty g) $ traceShow previous $ filter (/= previous) (traceShowId $ nub $ reflects =<< gs)

solve :: (Grid DotHash -> [Either Int Int]) -> Int
solve f =
  $(input 13)
    & unpack
    & splitOn "\n\n"
    & fmap pack
    & fmap readGrid
    & fmap f
    & mconcat
    & partitionEithers
    & both sum
    & second (* 100)
    & uncurry (+)

part1 :: Int
part1 = solve reflects

part2 :: Int
part2 = solve reflectsSmudged