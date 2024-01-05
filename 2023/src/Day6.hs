module Day6 (part1, part2) where

asMany :: Parser [(Double, Double)]
asMany = do
  let l s = (string (s <> ":") >> whitespace) *> (number `sepBy` whitespace)
  zip <$> (l "Time" <* eol) <*> (l "Distance" <* eof)

asOne :: Parser (Double, Double)
asOne = asMany <&> (unzip >>> both (fmap (round >>> show) >>> mconcat >>> uread))

ways :: (Double, Double) -> ℤ'
ways (t, d) =
  let f pm = ((-t `pm` sqrt (t ** 2 - 4 * d)) / (-2))
   in ceiling (f (+)) - floor (f (-)) - 1

part1 :: ℤ'
part1 = $(input 6) ⊢ asMany & fmap ways & (ȣ ∏)

part2 :: ℤ'
part2 = $(input 6) ⊢ asOne & ways
