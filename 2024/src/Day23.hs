module Day23 (part1, part2) where

import Data.List (dropWhileEnd)
import Relude.Unsafe qualified as U

clique :: ([Set (Set String)] -> Set (Set String)) -> Set (Set String)
clique f =
  let g' = $(aoc 23) |- (mapcat "-" abc abc)
      g = mk <$> mkWith (∪) ([[(a, mk₁ b), (b, mk₁ a)] | (a, bs) <- unMap g', b <- bs] <>!)
      cls = f (cliques g)
   in mk [mk cl | cl@[x : _, y : _, z : _] <- un <$> un cls, 't' ∈ [x, y, z]]

clique' :: ([Set (Set String)] -> Set (Set String)) -> Set (Set String)
clique' f =
  let g' = $(aoc 23) |- (mapcat "-" abc abc)
      g = mk <$> mkWith (∪) ([[(a, mk₁ b), (b, mk₁ a)] | (a, bs) <- unMap g', b <- bs] <>!)
   in f (cliques g)

cliques :: (String :|-> Set String) -> [Set (Set String)]
cliques g =
  takeWhile (not ∘ null) $
    scanl' (\cs _ -> setConcatMap expand cs) (mk (mk₁ <$> keys g)) [0 .. size g]
  where
    expand cs = mk [c |-> cs | c <- keys g, all ((c ∈) ∘ (g |!)) cs]

part1 :: ℤ
part1 = size (clique (!! 2))

part2 = intercalate "," (sort (un (setConcat (clique' (\cls -> cls !! (size cls - 1))))))
