module Day13 (part1, part2) where

reflects :: STUArrayGrid s Char -> ST s (Set (Either ℤ' ℤ'))
reflects g = do
  (maxX, maxY) <- maxXYM g
  let b m0 m1 i = let w = min i (m0 - i + 1) in (([i - w .. i - 1], [0 .. m1]), ([i + w - 1, i + w - 2 .. i], [0 .. m1]))
  ((⊏⊐) <$>) . (fst <$$>) $
    filterM
      (snd >>> bothM (\(xr, yr) -> sequence [g <||!> (x, y) | x <- xr, y <- yr]) >>> fmap (uncurry (==)))
      [(f i, s (b m0 m1 i)) | (f, (m0, m1), s, ir) <- [(Left, (maxX, maxY), id, [1 .. maxX]), (Right, (maxY, maxX), both swap, [1 .. maxY])], i <- ir]

reflectsSmudged :: STUArrayGrid s Char -> ST s (Set (Either ℤ' ℤ'))
reflectsSmudged g = do
  let toggle g c = void $ g <||~> (c, bool '.' '#' . (== '.'))
  rs' <- traverse (\c -> toggle g c *> reflects g <* toggle g c) =<< coordsM g
  (setFilter <$> ((∌) <$> reflects g)) <*> pure (λ ⋃ rs')

solve :: (STUArrayGrid s Char -> ST s (Set (Either ℤ' ℤ'))) -> ST s [STUArrayGrid s Char] -> ST s ℤ'
solve f = fmap (uncurry (+) . second (* 100) . both (λ ∑) . partitionEithers . concatMap co) . (traverse f =<<)

part1 :: ℤ'
part1 = runST $ solve reflects $(gridsM input 13)

part2 :: ℤ'
part2 = runST $ solve reflectsSmudged $(gridsM input 13)