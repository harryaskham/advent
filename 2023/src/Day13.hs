module Day13 (part1, part2) where

reflects :: STUArrayGrid s Char -> ST s (Set (Either ℤ' ℤ'))
reflects g' = do
  v <- r270 <$> variantsM g'
  co . concat
    <$> forM
      [(Left, g'), (Right, v)]
      ( \(f, g) -> do
          (maxX, maxY) <- maxXYM g
          ixs <-
            filterM
              ( \i -> do
                  let w = min i (maxX - i + 1)
                  l <- sequence [g <||!> (x, y) | x <- [i - w .. i - 1], y <- [0 .. maxY]]
                  r <- sequence [g <||!> (x, y) | x <- reverse [i .. i + w - 1], y <- [0 .. maxY]]
                  return $ l == r
              )
              [1 .. maxX]
          return $ f <$> ixs
      )

reflectsSmudged :: STUArrayGrid s Char -> ST s (Set (Either ℤ' ℤ'))
reflectsSmudged g = do
  cs <- coordsM g
  traceShowM (length cs)
  rs <- reflects g
  let toggle g cM =
        case cM of
          Nothing -> return ()
          Just c -> void $ g <||~> (c, bool '.' '#' . (== '.'))
  rs' <- forM (pairs (Nothing : (Just <$> cs))) $ \(last, c) -> do
    toggle g last
    toggle g c
    reflects g
  return $ setFilter (∉ rs) (λ ⋃ rs')

solve :: (STUArrayGrid s Char -> ST s (Set (Either ℤ' ℤ'))) -> ST s [STUArrayGrid s Char] -> ST s ℤ'
solve f gsM = do
  gs <- gsM
  uncurry (+) . second (* 100) . both (λ ∑) . partitionEithers . concatMap co <$> traverse f gs

part1 :: ℤ'
part1 = runST $ solve reflects $ $(gridsM input 13)

part2 :: ℤ'
part2 = runST $ solve reflectsSmudged $ $(gridsM input 13)
