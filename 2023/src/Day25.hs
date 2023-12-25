module Day25 (part1, part2) where

part1 =
  $(input 25)
    |- (let s = count 3 alphaNum in many1 ((,) <$> (s <* string ": ") <*> (s `sepBy` char ' ') <* eol) <* eof)
    & mkMap
    & (id &&& swapMapList)
    & uncurry (unionWith (<>))

part2 :: Text
part2 = "Merry Christmas!"
