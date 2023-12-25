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
addAll g = go $ mkSeq [((∅), (∅), [], [(min a b, max a b) | (a, bs) <- unMap g, b <- bs])]
  where
    go ((left, right, [_,_,_], []) :<| _) =  product (length <$> [left, right])
    go ((_, _, _, []) :<| q) = go q
    go ((left, right, failed, ((a,b):edges)) :<| q)
      | length failed > 3 = go q
      | (a ∈ left && b ∈ left) || (a ∈ right && b ∈ right) = go (q |> (left, right, failed, edges))
      | (a ∈ left && b ∈ right) || ( a ∈ right && b ∈ left ) = go (q |> (left, right, (a,b):failed, edges))
      | (a ∈ left && b ∈ right) || (a ∈ right && b ∈ left) = go (q |> (left, right, (a,b):failed, edges))
      | a ∈ left = go (q |> (b |->left, right, failed, edges) |> (left, right, (a,b):failed, edges))
      | a ∈ right = go (q |> (left, b|->right, failed, edges) |> (left, right, (a,b):failed, edges))
      | b ∈ left = go (q |> (a |->left, right, failed, edges) |> (left, right, (a,b):failed, edges))
      | b ∈ right = go (q |> (left, a|->right, failed, edges) |> (left, right, (a,b):failed, edges))
      | otherwise = go (q |> (a |-> (b|-> left), right, failed, edges) |> (left, a|->(b|->right), failed, edges) |> (left, right, (a,b):failed, edges))


part1 :: Int
part1 =
  -- \$(input 25)
  $(exampleInput 25)
    |- (let s = count 3 alphaNum in many1 ((,) <$> (s <* string ": ") <*> (s `sepBy` char ' ') <* eol) <* eof)
    & mkMap
    & (id &&& swapMapList)
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
