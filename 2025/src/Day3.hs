module Day3 (part1, part2) where

j :: Integer -> Vector ℕ₁₀ -> ℤ
j e bs =
  let n = size bs
      go :: (ℤ, ℤ) .->. Maybe ℤ
      go (i, e)
        | e == 0 = pure (Just 0)
        | i == n = pure Nothing
        | otherwise = do
            aM <- go .$. (i + 1, e)
            bM <- go .$. (i + 1, e - 1)
            case bM of
              Nothing -> pure aM
              Just b' ->
                let b = b' + (fromIntegral (bs !! i) * 10 ^ (e - 1))
                 in case aM of
                      Nothing -> pure . Just $ b
                      Just a -> pure . Just $ max a b
   in (? 0) . run $ go .$. (0, e)

(part1 :: ℤ, part2 :: ℤ) = both (\e -> $(aoc 3) |-<..> (Σ . j e . co <$> many nat₁₀)) (2, 12)
