module Day13 (part1, part2) where

reflects :: STUArrayGrid s DotHash -> Set (Either ℤ' ℤ')
reflects g' = runST do
  v <- r270 <$> variantsM g'
  co . concat <$> forM
    [(Left, g'), (Right, v)]
    ( \(f, g) -> do
        maxX <- fst <$> maxXYM g
        forM
          [1 .. maxX]
          ( \i -> do
              let w = min i (maxX - i + 1)
              guardM do
                (l, r) <- partitionCoordsM (< (i, 0)) g
                (l', r') <- bimapM (cropXM (i - w) i) (cropXM 0 w . mapCoords (first (subtract i))) (l, r)
                let (l'', r'') = sortT2On (length . coords) (l', r')
                rV <- v0 <$> variantsM r''
                return $ l'' == rV
              return $ f i
          )
    )

reflectsSmudged :: STUArrayGrid s DotHash -> Set (Either ℤ' ℤ')
reflectsSmudged g =
  setFilter
    (∉ reflects g)
    (λ ⋃ [reflects (g |~ (c, bool Dot Hash . (== Dot))) | c <- coords g])

solve :: (STUArrayGrid s DotHash -> Set (Either ℤ' ℤ')) -> [STUArrayGrid s DotHash] -> ℤ'
solve f = concatMap (co . f) >>> partitionEithers >>> both sum >>> second (* 100) >>> uncurry (+)

part1 :: ℤ'
part1 = solve reflects $(grids input 13)

part2 :: ℤ'
part2 = solve reflectsSmudged $(grids input 13)
