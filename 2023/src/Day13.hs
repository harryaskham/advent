module Day13 (part1, part2) where

reflects :: IntMapGrid Char -> Identity (Set (Either ℤ' ℤ'))
reflects g = do
  (maxX, maxY) <- maxXYM g
  let b m0 m1 i = let w = min i (m0 - i + 1) in (([i - w .. i - 1], [0 .. m1]), ([i + w - 1, i + w - 2 .. i], [0 .. m1]))
  ((⊏⊐) <$>) . (fst <$$>) $
    filterM
      (snd >>> bothM (\(xr, yr) -> sequence [g <||!> (x, y) | x <- xr, y <- yr]) >>> fmap (uncurry (==)))
      [(f i, s (b m0 m1 i)) | (f, (m0, m1), s, ir) <- [(Left, (maxX, maxY), id, [1 .. maxX]), (Right, (maxY, maxX), both swap, [1 .. maxY])], i <- ir]

reflectsSmudged :: IntMapGrid Char -> Identity (Set (Either ℤ' ℤ'))
reflectsSmudged g = do
  let toggle c g = g <||~> (c, bool '.' '#' . (== '.'))
  rs' <- traverse (\c -> (toggle c g >>= reflects) <* toggle c g) =<< coordsM g
  (setFilter <$> ((∌) <$> reflects g)) <*> pure (λ (⋃ rs'))

solve :: (IntMapGrid Char -> Identity (Set (Either ℤ' ℤ'))) -> Identity [IntMapGrid Char] -> Identity ℤ'
solve f = fmap (uncurry (+) . second (* 100) . both sum . partitionEithers . concatMap co) . (traverse f =<<)

part1 :: ℤ'
part1 = runIdentity $ solve reflects $(gridsM input 13)

part2 :: ℤ'
part2 = runIdentity $ solve reflectsSmudged $(gridsM input 13)
