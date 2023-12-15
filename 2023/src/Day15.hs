module Day15 (part1, part2) where

hash :: String -> Int
hash = foldl' (\n c -> ((n + ord c) * 17) `mod` 256) 0

part1 :: Int
part1 = $(input 15) |- (many (noneOf ",") `sepBy` char ',') & fmap hash & sum

part2 :: Int
part2 =
  let op = (,) <$> many1 alphaNum <*> (((,) <$> char '=' <*> number) <|> ((,) <$> char '-' <*> pure 0))
      run m o = m |~ (hash (fst o), runOp o)
      runOp (l, ('=', f)) (m, s) = bool ((m |. (l, f)), (s |> l)) ((m |. (l, f)), s) (l |∈ m)
      runOp (l, ('-', _)) b@(m, s) = bool b ((m |/ l), (s >/< l)) (l |∈ m)
      power (i, (m, s)) = sum [(i + 1) * slot * (m |! l) | (slot, l) <- zip [1 ..] (unSeq s)]
      boxes = mkMap [(i, ((mkMap []), (mkSeq []))) | i <- [0 .. 255]]
   in $(input 15) |- (op `sepBy` char ',') & (foldl' run boxes >>> unMap >>> fmap power >>> sum)
