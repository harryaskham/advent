module Day25 (part1, part2) where

traverseWithout :: Set (String, String) -> Map String [String] -> Int
traverseWithout abc g = go (∅) (mkSeq (take 1 (keys g)))
  where
    go seen Empty = size seen
    go seen (a :<| q)
      | a ∈ seen = go seen q
      | otherwise = go (a |-> seen) (q >< mkSeq ([b | b <- fromMaybe [] $ g |? a, (a, b) ∉ abc, (b, a) ∉ abc]))

part1 :: Int
part1 =
  $(input 25)
    |- (let s = count 3 alphaNum in many1 ((,) <$> (s <* string ": ") <*> (s `sepBy` char ' ') <* eol) <* eof)
    & mkMap
    & (id &&& swapMapList)
    & uncurry (unionWith (<>))
    & (traverseWithout (mkSet [("vfx", "bgl"), ("bqq", "rxt"), ("btp", "qxr")]) &&& size)
    & (\(a, b) -> a * (b - a))

part2 :: Text
part2 = "Merry Christmas!"
