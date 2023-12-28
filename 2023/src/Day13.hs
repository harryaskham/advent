module Day13 (part1, part2) where

reflects :: STUArrayGrid s Char -> ST s (Set (Either ℤ' ℤ'))
reflects g' = do
  v <- r270 <$> variantsM g'
  co . catMaybes . concat <$> forM
    [(Left, g'), (Right, v)]
    ( \(f, g) -> do
        (maxX, maxY) <- maxXYM g
        forM
          [1 .. maxX]
          ( \i -> do
              let w = min i (maxX - i + 1)
                  rc (x, y) = (i + w - x, y)
              l <- sequence [g <||!> (x, y) | x <- [0 .. w - 1], y <- [0 .. maxY]]
              r <- sequence [g <||!> (x, y) | x <- reverse [w .. w +i- 1], y <- [0 .. maxY]]
              return if l == r then Just (f i) else Nothing
              -- (l, r) <- partitionCoordsM (< (i, 0)) g
              -- (l', r') <- bimapM (cropXM (i - w) i) (mapCoordsM (first (subtract i)) >=> cropXM 0 w ) (l, r)
              -- (l'', r'') <- sortT2OnM (fmap length . coordsM) (l', r')
              -- rV <- v0 <$> variantsM r''
              -- return $ if l'' == rV then Just (f i) else Nothing
          )
    )

reflectsSmudged :: STUArrayGrid s Char -> ST s (Set (Either ℤ' ℤ'))
reflectsSmudged g = do
  cs <- coordsM g
  traceShowM (length cs)
  rs <- reflects g
  let toggle g cM =
        case cM of
          Nothing -> return ()
          Just c -> traceShow c $ void $ g <||~> (c, bool '.' '#' . (== '.'))
  rs' <- forM (pairs (Nothing:(Just <$> cs))) $ \(last, c) -> traceShow c do
    toggle g last
    toggle g c
    reflects g
  return $ setFilter (∉ rs) (λ ⋃  rs')

solve :: (STUArrayGrid s Char -> ST s (Set (Either ℤ' ℤ'))) -> ST s [STUArrayGrid s Char] -> ST s ℤ'
solve f gsM = do
  gs <- gsM
  traceShowM (length gs)
  uncurry (+) . second (*100) . both (λ ∑) . partitionEithers . concatMap co <$> traverse f gs

part1 :: ℤ'
part1 = runST $ solve reflects $(gridsM input 13)

part2 :: ℤ'
part2 = runST $ solve reflectsSmudged $(gridsM input 13)
