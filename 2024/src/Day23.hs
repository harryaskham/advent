module Day23 (part1, part2) where

part1 :: ℤ
part1 =
  let g' = $(aoc 23) |- (mapcat "-" abc abc)
      g = mk <$> mkWith (<>) ([[(a, [b]), (b, [a])] | (a, bs) <- unMap g', b <- bs] <>!)
      cls = cliques g !! 2
   in size $ nub [sort cl | cl@[x : _, y : _, z : _] <- un <$> cls, 't' ∈ [x, y, z]]

cliques :: (String :|-> Set String) -> [[Set String]]
cliques g = scanl' (\cls _ -> nub ∘ sort $ expand =<< cls) (mk . pure <$> keys g) [0 ..]
  where
    expand cs = [c |-> cs | c <- keys g, all ((c ∈) ∘ (g |!)) cs]

{-
There are still way too many results to go through them all. You'll have to find the LAN party another way and go there yourself.

Since it doesn't seem like any employees are around, you figure they must all be at the LAN party. If that's true, the LAN party will be the largest set of computers that are all connected to each other. That is, for each computer at the LAN party, that computer will have a connection to every other computer at the LAN party.

In the above example, the largest set of computers that are all connected to each other is made up of co, de, ka, and ta. Each computer in this set has a connection to every other computer in the set:

ka-co
ta-co
de-co
ta-ka
de-ta
ka-de
The LAN party posters say that the password to get into the LAN party is the name of every computer at the LAN party, sorted alphabetically, then joined together with commas. (The people running the LAN party are clearly a bunch of nerds.) In this example, the password would be co,de,ka,ta.

What is the password to get into the LAN party?
-}

part2 :: Text
part2 = "Part 2"

fill :: (String :|-> [String]) -> (String :|-> Set String)
fill g =
  let go Empty seen = seen
      go (c :<| q) seen
        | c ∈ seen = go q seen
        | otherwise = go (q >< mk (g |! c)) (c |-> seen)
   in mkMap [(s, go (mk₁ s) ø) | s <- nub (keys g : values g <>!)]
