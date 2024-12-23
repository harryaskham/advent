module Day23 (part1, part2) where

clique :: NonEmpty (Set (Set String))
clique =
  cliques $
    mk
      <$> mkWith
        (∪)
        ( [ [(a, mk₁ b), (b, mk₁ a)]
            | (a, bs) <- unMap ($(aoc 23) |- (mapcat "-" abc abc)),
              b <- bs
          ]
            <>!
        )

cliques :: (String :|-> Set String) -> NonEmpty (Set (Set String))
cliques g = cl :| cls
  where
    expand cs =
      mk
        [ c |-> cs
          | c <- keys g,
            all ((c ∈) ∘ (g |!)) cs
        ]
    (cl : cls) =
      takeWhile (not ∘ null) $
        scanl'
          (\cs _ -> setConcatMap expand cs)
          (mk (mk₁ <$> keys g))
          [0 .. size g]

part1 :: Σ ℤ
part1 =
  ( [ Σ 1
      | [x : _, y : _, z : _] <- un <$> un (clique !! (2 :: ℤ)),
        't' ∈ [x, y, z]
    ]
      <>!
  )

part2 :: String
part2 = intercalate "," ∘ sort ∘ un ∘ setConcat ∘ last $ clique
