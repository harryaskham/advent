module Day25 (part1, part2) where

cliquesWithout :: (String, String) -> (String, String) -> (String, String) -> Map String [String] -> [[String]]
cliquesWithout a b c g = go (mkSet (keys g)) [] [] (mkSeq []) (mkSet [])
  where
    go remaining current cliques Empty seen
      | null remaining = filter (not . null) (current : cliques)
      | otherwise =
          let next = (uhead $ unSet remaining)
           in go (remaining \\ mkSet [next]) [] (current : cliques) (mkSeq [next]) seen
    go remaining current cliques (here :<| q) seen =
      let banned = [a, b, c, swap a, swap b, swap c]
          theres = [there | there <- fromMaybe [] $ g |? here, (here, there) ∉ banned, (here, there) ∉ seen]
          seen' = foldl' (<-|) seen ((here,) <$> theres)
       in go remaining (here : current) cliques (q >< mkSeq theres) seen'

part1 :: Int
part1 =
  $(exampleInput 25)
    |- (let s = count 3 alphaNum in many1 ((,) <$> (s <* string ": ") <*> (s `sepBy` char ' ') <* eol) <* eof)
    & mkMap
    & (id &&& swapMapList)
    & uncurry (unionWith (<>))
    & ( \g -> uhead do
          (a, b, c) <- triTriples (keys g)
          (a', b', c') <- triTriples (keys g)
          guard $ a /= a' && b /= b' && c /= c'
          let cliques = cliquesWithout (a, a') (b, b') (c, c') g
          guard $ length cliques == 2
          return $ product (length <$> cliques)
      )

part2 :: Text
part2 = "Merry Christmas!"
