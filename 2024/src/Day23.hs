module Day23 (part1, part2) where

import Data.List (dropWhileEnd)

clique :: ([Set (Set String)] -> Set (Set String)) -> Set (Set String)
clique f =
  let g' = $(aocx 23) |- (mapcat "-" abc abc)
      g = mk <$> mkWith (∪) ([[(a, mk₁ b), (b, mk₁ a)] | (a, bs) <- unMap g', b <- bs] <>!)
      cls = f (cliques g)
   in mk [mk cl | cl@[x : _, y : _, z : _] <- un <$> un cls, 't' ∈ [x, y, z]]

cliques :: (String :|-> Set String) -> [Set (Set String)]
cliques g = scanl' (\cs _ -> setConcatMap expand cs) (mk (mk₁ <$> keys g)) [0 ..]
  where
    expand cs =
      -- traceShow ("expand", cs) $
      mk [c |-> cs | c <- keys g, all ((c ∈) ∘ (g |!)) cs]

takeChanged :: [Set (Set String)] -> Set (Set String)
takeChanged (x:xs)
  | size x == 1 = x
  | otherwise = traceShow xs $ takeChanged xs

part1 :: ℤ
part1 = size (clique (!! 2))

part2 :: String
part2 = traceShowId (setConcat (clique takeChanged)) & un & traceShowId & traceShow "wtf" & sort & intercalate ","
