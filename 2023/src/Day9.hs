module Day9 (part1, part2) where

solve :: ([Int] -> Int) -> (Int -> Int -> Int) -> String -> Int
solve get pm = parseWith (many1 ((number `sepBy` string " ") <* eol) <* eof) >>> fmap (pure &&& enumFromTo 2 . length >>> uncurry (foldl' (\(ds : dss) _ -> (uncurry (-) <$> zip (drop 1 ds) ds) : ds : dss)) >>> (fmap get >>> ((foldr1 pm . utail) &&& uhead) >>> uncurry (flip pm))) >>> sum

part1 :: Int
part1 = solve ulast (+) $(input 9)

part2 :: Int
part2 = solve uhead (-) $(input 9)