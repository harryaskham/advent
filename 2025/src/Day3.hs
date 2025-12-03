module Day3 (part1, part2) where

p :: Integer -> Vector ℕ₁₀ -> ℤ
p e bs =
  let n = size bs
      go :: (ℤ, ℤ) .->. Maybe ℤ
      go (i, e)
        | e == 0 = pure (Just 0)
        | i == n = pure Nothing
        | otherwise = do
            aM <- go .$. (i + 1, e)
            bM <- go .$. (i + 1, e - 1)
            case (aM, bM) of
              (Nothing, Nothing) -> pure Nothing
              (Just a, Nothing) -> pure . Just $ a
              (Nothing, Just b') ->
                let b = b' + (fromIntegral (bs !! i) * 10 ^ (e - 1))
                 in pure . Just $ b
              (Just a, Just b') ->
                let b = b' + (fromIntegral (bs !! i) * 10 ^ (e - 1))
                 in pure . Just $ max a b
   in (? 0) . run $ go .$. (0, e)

part1 :: Integer
part1 = $(aoc 3) |-<..> (Σ . p 2 . co <$> many nat₁₀)

part2 :: Integer
part2 = $(aoc 3) |-<..> (Σ . p 12 . co <$> many nat₁₀)
