module Day15 (part1, part2) where

hash :: String -> ℤ'
hash = foldl' (\n c -> (n + ord c) * 17 `mod` 256) 0

part1 :: ℤ'
part1 = $(input 15) ⊢ csv & fmap hash & sum

part2 :: ℤ'
part2 =
  let op = (,) <$> many1 alphaNum <*> ((,) <$> char '=' <*> number <|> (,) <$> char '-' <*> pure 0)
      run m o = m |~ (hash (fst o), runOp o)
      runOp (l, ('=', f)) (m, s) = bool (m |. (l, f), s |> l) (m |. (l, f), s) (l ∈ m)
      runOp (l, ('-', _)) b@(m, s) = bool b (m |/ l, s >/< l) (l ∈ m)
      power (i, (m, s)) = sum [(i + 1) * slot * (m |! l) | (slot, l) <- zip [1 ..] (unSeq s)]
      boxes = mkMap [(i, (mkMap [], mkSeq [])) | i <- [0 .. 255]]
   in $(input 15) ⊢ (op `sepBy` char ',') & (foldl' run boxes >>> unMap >>> fmap power >>> sum)
