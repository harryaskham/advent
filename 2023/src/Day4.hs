module Day4 (part1, part2) where

parser :: Parser [(ℤ', [ℤ'], [ℤ'])]
parser = many1 (line <* eol) <* eof
  where
    gap = many $ string " "
    line = do
      i <- (string "Card" >> gap) *> number <* (string ":" >> gap)
      as <- many1 (number <* gap) <* (string "|" >> gap)
      bs <- number `sepBy1` gap
      return (i, as, bs)

part1 :: ℤ'
part1 =
  $(input 4)
    & parseWith parser
    & fmap (\(_, as, bs) -> 2 ^ size (as ∩ bs) `div` 2)
    & sum

part2 :: ℤ'
part2 =
  $(input 4)
    |- parser
    & ( \cards ->
          foldl'
            ( \m (i, as, bs) ->
                let n = size (as ∩ bs)
                    c = fromMaybe 0 $ m |? i
                 in foldl' (flip (adjust (+ c))) m [i + 1 .. i + n]
            )
            (mkMap [(i, 1) | (i, _, _) <- cards])
            cards
      )
    & unMap
    & fmap snd
    & sum
