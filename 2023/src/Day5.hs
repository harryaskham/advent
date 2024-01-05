module Day5 (part1, part2) where

parser :: Parser ([ℤ], [ℤ], [(ℤ -> ℤ, ℤ -> ℤ)])
parser = do
  seeds <- string "seeds: " *> (number `sepBy1` string " ") <* (eol >> eol)
  (mappings, destinations) <- unzip <$> (mapping `sepBy1` eol) <* eof
  return (seeds, concat destinations, mappings)
  where
    rangeLine = do
      (dest, source) <- let n = (number <* char ' ') in (,) <$> n <*> n
      rangeSize <- number <* eol
      return
        ( toTuple2 $
            [(source, dest), (dest, source)]
              <&> ( \(a, b) x ->
                      if (x >= a) && (x < a + rangeSize)
                        then Just $ b + (x - a)
                        else Nothing
                  ),
          dest
        )
    mapping = do
      many1 (noneOf "\n") >> eol
      (fs, destinations) <- unzip <$> many1 rangeLine
      let forwardBackward =
            toTuple2 $
              [fst, snd]
                <&> ( \f x -> case catMaybes (f <$> fs <*> pure x) of
                        [] -> x
                        (x' : _) -> x'
                    )
      return (forwardBackward, destinations)

part1 :: ℤ
part1 =
  $(input 5)
    ⊢ parser
    & (\(seeds, _, mappings) -> flip (foldl' (flip fst)) mappings <$> seeds)
    & minimum

part2 :: ℤ
part2 =
  $(input 5)
    ⊢ parser
    & ( \(seeds, destinations, mappings) ->
          let validSeed seed = λ (⋁ [(seed >= a) && (seed < a + b) | [a, b] <- chunksOf 2 seeds])
           in [d | d <- destinations, validSeed $ Ɍ ($) d ˙ (snd <$> mappings)]
      )
    & minimum
