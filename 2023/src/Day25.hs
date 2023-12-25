module Day25 (part1, part2) where

traverseWithout abc g = go (∅) (mkSeq (take 1 (keys g)))
  where
    go seen Empty = size seen
    go seen (a :<| q)
      | a ∈ seen = go seen q
      | otherwise = go (a |-> seen) (q >< mkSeq ([b | b <- fromMaybe [] $ g |? a, (a, b) ∉ abc, (b, a) ∉ abc]))

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
   in -- in go (∅) (mkSeq [(mkSet [], mkSet [], [(a, b), (c, d)], delete (a, b) . delete (c, d) $ edges) | (a, b) <- edges, (c, d) <- edges, a /= c && a /= d && b /= c && b /= d])
      go (∅) (mkSeq [((∅), (∅), [], edges)])
  where
    go seen ((left, right, failed@[_, _, _], []) :<| q)
      | left == (∅) || right == (∅) = go (failed |-> seen) q
      | otherwise = product (length <$> [left, right])
    go seen ((_, _, failed, []) :<| q) = go (failed |-> seen) q
    -- go seen ((_, _, failed, []) :<| q) = go seen q
    go seen ((left, right, failed, edge : edges) :<| q)
      -- \| length failed > 3 = go seen q
      -- \| key ∈ seen = go seen q
      | failed ∈ seen = go seen q
      | a ∈ left && b ∈ left || a ∈ right && b ∈ right = go seen' ((left, right, failed, edges) :<| q)
      | a ∈ left && b ∈ right || a ∈ right && b ∈ left = go seen' (doFail q)
      | a ∈ left || b ∈ left = go seen' (doFail $ q :|> (a |-> (b |-> left), right, failed, edges))
      | a ∈ right || b ∈ right = go seen' (doFail $ q :|> (left, a |-> (b |-> right), failed, edges))
      | otherwise = go seen' (doFail $ (a |-> (b |-> left), right, failed, edges) :<| (q :|> (left, a |-> (b |-> right), failed, edges)))
      where
        (a, b) =
          traceShow (size left, size right, size failed, size edges) $
            edge
        doFail = if length failed < 3 then ((left, right, (a, b) : failed, edges) :<|) else id
        seen' = seen

findTriple :: [(String, String)] -> Int
findTriple edges =
  traceShow (length edges) $
    uhead $
      catMaybes [go (traceShow (i, j, k) $ mkSet [a, b, c]) (mkSet edges) | (i, a) <- zip [0 ..] edges, (j, b) <- zip [0 ..] edges, (k, c) <- zip [0 ..] edges, i /= j && j /= k && k /= i]
  where
    go :: Set (String, String) -> Set (String, String) -> Maybe Int
    go without edges =
      let f Nothing = Nothing
          f (Just (l, r)) =
            foldl'
              ( \(Just (l, r)) (a, b) ->
                  if
                      | (a, b) ∈ without -> Just (l, r)
                      | a ∈ l && b ∈ l || a ∈ r && b ∈ r -> Just (l, r)
                      | a ∈ l && b ∈ r || a ∈ r && b ∈ l -> Nothing
                      | a ∈ l || b ∈ l -> Just ((a |-> (b |-> l)), r)
                      | a ∈ r || b ∈ r -> Just (l, (a |-> (b |-> r)))
                      | otherwise -> Just (l, r)
              )
              (Just (l, r))
              edges
          lrM = iterateFix f (Just ((∅), (∅)))
       in case lrM of
            Nothing -> Nothing
            Just (left, right) -> if left == (∅) || right == (∅) then Nothing else Just (size left * size right)

-- u can remove anything thta is connected to things

part1 :: Int
part1 =
  $(exampleInput 25)
    -- \$(input 25)
    |- (let s = count 3 alphaNum in many1 ((,) <$> (s <* string ": ") <*> (s `sepBy` char ' ') <* eol) <* eof)
    & mkMap
    & id
    &&& swapMapList
    & uncurry (unionWith (<>))
    & traceShowId
    -- & (\g -> nub [(min a b, max a b) | (a, bs) <- unMap g, b <- bs])
    -- & findTriple
    & addAll

-- & ( \g -> uhead do
--       let edges = nub [(min a b, max a b) | (a, bs) <- unMap g, b <- bs]
--       (i, a) <- zip [0 ..] edges
--       (j, b) <- zip [0 ..] edges
--       (k, c) <- zip [0 ..] edges
--       traceShow (i, j, k) $ guard $ a /= b && b /= c && c /= a
--       -- let cliques = cliquesWithout a b c g
--       let n = traverseWithout [a, b, c] g
--       guard $ traceShowId n /= size g
--       return $ n * (size g - n)
--   )

part2 :: Text
part2 = "Merry Christmas!"
