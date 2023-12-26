module Day9 (part1, part2) where

solve :: ([ℤ'] -> ℤ') -> (ℤ' -> ℤ' -> ℤ') -> String -> ℤ'
solve get pm = parseWith (many1 ((number `sepBy` string " ") <* eol) <* eof) >>> fmap (pure &&& enumFromTo 2 . length >>> uncurry (foldl' (\(ds : dss) _ -> (uncurry (-) <$> zip (drop 1 ds) ds) : ds : dss)) >>> (fmap get >>> ((foldr1 pm . utail) &&& uhead) >>> uncurry (flip pm))) >>> sum

part1 :: ℤ'
part1 = solve ulast (+) $(input 9)

part2 :: ℤ'
part2 = solve uhead (-) $(input 9)