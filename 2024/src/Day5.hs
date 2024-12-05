module Day5 where

inp :: ((Map Integer [Integer], Map Integer [Integer]), [[Integer]])
inp =
  $(input 5)
    |- ( (,)
           <$> ( (both (mkMapWith (<>) . fmap (second pure)) . second (fmap swap) . dup)
                   <$> ((many1 ((,) <$> (number @Integer <* char '|') <*> (number @Integer <* eol))) <* eol)
               )
           <*> (many1 ((number @Integer `sepBy1` char ',') <* eol))
       )

mkCompare :: (Map Integer [Integer], Map Integer [Integer]) -> Integer -> Integer -> Ordering
mkCompare (lt, gt) a b
  | a == b = EQ
  | (a `elem` (gt |? b ? [a])) && (b `elem` (lt |? a ? [b])) = LT
  | otherwise = GT

part1 :: Integer
part1 =
  inp
    & ( \(rules, orders) ->
          zipWith (\a b -> bool 0 (middle a) (a == b)) orders (sortBy (mkCompare rules) <$> orders)
      )
    & sum

part2 :: Integer
part2 =
  inp
    & ( \(rules, orders) ->
          zipWith (\a b -> bool (middle b) 0 (a == b)) orders (sortBy (mkCompare rules) <$> orders)
      )
    & sum
