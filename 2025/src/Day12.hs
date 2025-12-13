module Day12 (part1, part2) where

(ps, rs) :: [(ℤ, ".#" ▦ ℤ²)] × [(ℤ², [ℤ])] =
  $(aoc 12) & (⊏|⊐) @(([(ℤ, ".#" ▦ ℤ²) ⯻ ":\n"] ≠ []) × ([(ℤ² ⯻ "x", [ℤ] ⯻ " ") ⯻ ": "] ≠ []))

traceIt (w, h) s a =
  let g :: ".#" ▦ ℤ² = mkGrid [((x, y), if (x, y) ∈ s then (#"#" □) else (#"." □)) | y <- [0 .. h - 1], x <- [0 .. w - 1]]
   in traceTextLn (pretty g) a

place :: ℤ² -> Set (Set ℤ²) -> Set (Set ℤ²) -> Set (Set ℤ²)
place (w, h) gs vs =
  mk
    [ g ∪ v
    | traceShow ("states", (gs |.|)) True,
      g <- un gs,
      traceShow ("space", w ⋅ h - (g |.|), "size", ((head' (un vs)) |.|)) True,
      -- traceIt (w, h) g True,
      x <- [0 .. w - 1],
      y <- [0 .. h - 1],
      v' <- un vs,
      let v = setMap (bimap (+ x) (+ y)) v',
      w ⋅ h - (g |.|) ≥ (v |.|),
      -- traceIt (w, h) v True,
      v |-?-> (\(x, y) -> x < 0 ∨ x ≥ w ∨ y < 0 ∨ y ≥ h) ≡ (∅),
      g ∩ v ≡ (∅)
    ]

fit :: (ℤ², [ℤ]) -> 𝔹
fit ((w, h), ns) =
  let pss = mk <$> ((mk ∘ (|?> (#"#" □)) <$>) ∘ variantsNub ∘ snd <$> ps) ⤊* ns
      gs = foldl' (place (w, h)) (mk₁ (∅)) pss
   in traceShow [traceIt (w, h) (head' (un ps)) True | ps <- pss] $
        traceShowId $
          not ∘ null $
            gs

part1 :: ℤ = rs |?| fit'

part2 :: ℤ = 0

-- TODO: islands of gaps, ∪ those
search :: ℤ² -> [[Set ℤ²]] -> 𝔹
search (w, h) vss' =
  let loss (g, vss) = ((vss |.|), un g & unzip & both (dup >>> bimap minimum maximum >>> ((-) $@) >>> (^ 2)) & ((*) $@))
      -- let loss (g, vss) = un g & unzip & both (dup >>> bimap minimum maximum >>> ((-) $@) >>> (^ 2)) & ((*) $@)
      -- let loss (g, vss) = negate $ sum [s | vs <- vss, v <- vs, let s = (|.|) (gaps g v)]
      -- let loss (g, vss) = 0 -- (vss |.|)
      -- loss (g, vss) = negate $ sum [(|.|) [g ∩ b ≡ (∅) | b <- bs] | bs <- nub $ bounding <$$> vss]
      bounding v =
        let ((minX, maxX), (minY, maxY)) = un v & unzip & both (dup >>> bimap minimum maximum)
         in mkSet [(x, y) | x <- [minX .. maxX], y <- [minY .. maxY]]

      key (g, vss) = ((vss |.|), g)
      go _ _ NullQ = traceShowId False
      go _ seen ((_, st@(g, [])) :<! _) = traceShowId True
      go n seen ((_, st@(g, allVss@(vs : vss))) :<! q)
        -- \| key st ∈ seen = go seen q
        | n > 1000000 = False
        | otherwise =
            traceShow ("q", size q, "vss", size allVss, "n", n) $
              -- traceShow ("q", size q, "placed", (vss' |.|) - (allVss |.|)) $
              let states =
                    [ st'
                    | -- traceShow ("space", w ⋅ h - (g |.|), "size", ((head' (un vs)) |.|)) True,
                      v <- vs,
                      -- traceShow ("trying v", v, "bounding", bounding v) True,
                      -- not (terminate g v),
                      -- traceIt (w, h) g True,
                      -- traceIt (w, h) v True,
                      -- w ⋅ h - (g |.|) ≥ (v |.|),
                      g ∩ v ≡ (∅),
                      let st' = (g ∪ v, vss)
                    ]
                  q' = qAppend loss states q
                  seen' = key st |-> seen
               in go (n + 1) seen' q'
   in go 0 (∅) (mkQ₁ loss ((∅), vss'))

fit' :: (ℤ², [ℤ]) -> 𝔹
fit' ((w, h), ns) =
  let vs :: ".#" ▦ ℤ² -> [Set ℤ²]
      vs p =
        [ v
        | let vgs = variantsNub p,
          vg <- vgs,
          let vcs = vg |?> (#"#" □),
          x <- [0 .. w - 1],
          y <- [0 .. h - 1],
          let vcs' = (\(x', y') -> (x + x', y + y')) <$> vcs,
          let v = mk vcs',
          v |-?-> (\(x, y) -> x < 0 ∨ x ≥ w ∨ y < 0 ∨ y ≥ h) ≡ (∅)
        ]
      vss :: [[Set ℤ²]]
      vss = (vs <$> (sortOn (Down ∘ (|.|)) (snd <$> ps))) ⤊* ns
   in search (w, h) vss
