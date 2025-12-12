module Day12 (part1, part2) where

(ps, rs) :: [(ℤ, ".#" ▦ ℤ²)] × [(ℤ², [ℤ])] =
  $(aoc 12) & (⊏|⊐) @(([(ℤ, ".#" ▦ ℤ²) ⯻ ":\n"] ≠ []) × ([(ℤ² ⯻ "x", [ℤ] ⯻ " ") ⯻ ": "] ≠ []))

place :: ℤ² -> [".#" ▦ ℤ²] -> ".#" ▦ ℤ² -> [".#" ▦ ℤ²]
place (w, h) gs p =
  nub
    [ ((Ł (\g c -> g |. (c, (#"#" □))) g ks) !>)
    | g <- gs,
      traceTextLn (pretty g) $ True,
      x <- [0 .. w - 1],
      y <- [0 .. h - 1],
      v <- mapCoords (+ (x, y)) <$> variantsNub p,
      let ks = v |?> (#"#" □),
      let os = ks |-?-> ((≡ Just (#"." □)) ∘ (g |?)),
      os ≡ ks
    ]

fit :: (ℤ², [ℤ]) -> Maybe (".#" ▦ ℤ²)
fit ((w, h), ns) =
  let gs = foldl' (place (w, h)) [mkGrid [((x, y), (#"." □)) | x <- [0 .. w - 1], y <- [0 .. h - 1]]] ((snd <$> ps) ⤊* ns)
   in case gs of
        [] -> Nothing
        (g : _) -> Just g

part1 :: ℤ = rs |?| (isJust ∘ fit)

part2 :: ℤ = 0
