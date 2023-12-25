module Day25 (part1, part2) where

cliquesWithout :: (String, String) -> (String, String) -> (String, String) -> Map String [String] -> [[String]]
cliquesWithout a b c g = go (mkSet (keys g)) [] [] (mkSeq []) (mkSet [])
  where
    go remaining current cliques Empty seen
      | null remaining = nub <$> filter (not . null) (current : cliques)
      | otherwise =
          let next = (uhead $ unSet remaining)
           in go remaining [] (current : cliques) (mkSeq [next]) seen
    go remaining current cliques (here :<| q) seen =
      let banned = [a, b, c, swap a, swap b, swap c]
          theres = [there | there <- fromMaybe [] $ g |? here, (here, there) ∉ banned, (here, there) ∉ seen]
          seen' = foldl' (<-|) seen ((here,) <$> theres)
       in go (remaining \\ mkSet [here]) (here : current) cliques (q >< mkSeq theres) seen'

addAll :: Map String [String] -> Int
addAll g =
  let edges = nub [(min a b, max a b) | (a, bs) <- unMap g, b <- bs]
   in go (∅) (mkSeq [(mkSet [a, b], mkSet [c, d], [], delete (a, b) . delete (c, d) $ edges) | ((a, b), (c, d)) <- triPairs edges, a /= c && a /= d && b /= c && b /= d])
  where
    -- in go (∅) (mkSeq [((∅), (∅), [e], delete e edges) | e <- triTriples edges])

    go _ ((left, right, [_, _, _], []) :<| _) = product (length <$> [left, right])
    go seen ((_, _, failed, []) :<| q) = go (failed |-> seen) q
    -- go seen ((_, _, failed, []) :<| q) = go seen q
    go seen ((left, right, failed, edge : edges) :<| q)
      -- \| length failed > 3 = go seen q
      -- \| key ∈ seen = go seen q
      | failed ∈ seen = go seen q
      | a ∈ left && b ∈ left || a ∈ right && b ∈ right = go seen' ((left, right, failed, edges) :<| q)
      | a ∈ left && b ∈ right || a ∈ right && b ∈ left = go seen' (doFail q)
      | a ∈ left || b ∈ left = go seen' (doFail $ (a |-> (b |-> left), right, failed, edges) :<| q)
      | a ∈ right || b ∈ right = go seen' (doFail $ (left, a |-> (b |-> right), failed, edges) :<| q)
      | otherwise = go seen' (doFail $ (a |-> (b |-> left), right, failed, edges) :<| (q :|> (left, a |-> (b |-> right), failed, edges)))
      where
        (a, b) =
          traceShow (size left, size right, size failed, size edges) $
            edge
        doFail = if length failed < 3 then ((left, right, (a, b) : failed, edges) :<|) else id
        key = (left, right, failed)
        -- seen' = key |-> seen
        seen' = seen

part1 :: Int
part1 =
  $(exampleInput 25)
    -- \$(input 25)
    |- (let s = count 3 alphaNum in many1 ((,) <$> (s <* string ": ") <*> (s `sepBy` char ' ') <* eol) <* eof)
    & mkMap
    & id
    &&& swapMapList
    & uncurry (unionWith (<>))
    & addAll

-- & ( \g -> uhead do
--       (a, b, c) <- triTriples $ nub [(min a b, max a b) | (a, bs) <- unMap g, b <- bs]
--       guard $ a /= b && b /= c && c /= a
--       let cliques = cliquesWithout a b c g
--       guard $ traceShowId (length cliques) == 2
--       return $ product (length <$> cliques)
--   )

part2 :: Text
part2 = "Merry Christmas!"
