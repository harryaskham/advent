module Day12 (part1, part2) where

(ps, rs) :: [(ℤ, ".#" ▦ ℤ²)] × [(ℤ², [ℤ])] =
  $(aocx 12)
    -- \$(aoc 12)
    -- \$(aocxn 12 1)
    & (⊏|⊐) @(([(ℤ, ".#" ▦ ℤ²) ⯻ ":\n"] ≠ []) × ([(ℤ² ⯻ "x", [ℤ] ⯻ " ") ⯻ ": "] ≠ []))

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

fit :: (ℤ, (ℤ², [ℤ])) -> 𝔹
fit (ri, ((w, h), ns)) =
  let cs :: ℤ :|-> ℤ = mkMap $ enum ns
      traceG g a = traceTextLn (pretty g) a
      vs :: Vector (Set ℤ²) = mk [mk (p |?> (#"#" □)) | p <- snd <$> ps]
   in search ri (w, h) cs vs

search :: ℤ -> ℤ² -> ℤ :|-> ℤ -> Vector (Set ℤ²) -> 𝔹
search ri (w, h) cs vs =
  let loss (MaxSet _ free, cs) = size free
      key cache (mfree, cs) = (cs, head' $ sort (vars mfree))
      mfree = MaxSet (w - 1, h - 1) (box (0, 0) (w - 1, h - 1))
      -- toGrid :: Set ℤ² -> ".#" ▦ ℤ²
      -- toGrid v = mkGrid [(c, c ∈ v ??? (#"#" □) $ (#"." □)) | c <- un b]
      -- traceG g a = traceTextLn (pretty g) a
      -- traceV v a = traceG (toGrid v) a
      -- traceVs vs a = foldl' (\a v -> traceV v a) a (fst3 <$> un vs)
      -- traceVss vss a = foldl' (\a vs -> traceVs vs a) a (un vss)
      -- toGridI :: Set ℤ² -> ".#" ▦ ℤ²
      -- toGridI v = mkGrid [(c, c ∈ v ??? (#"." □) $ (#"#" □)) | c <- un b]
      -- traceGI g a = traceTextLn (pretty g) a
      -- traceVI v a = traceGI (toGridI v) a
      -- traceVsI vs a = foldl' (\a v -> traceVI v a) a (fst3 <$> un vs)
      -- traceVssI vss a = foldl' (\a vs -> traceVsI vs a) a (un vss)
      go _ _ _ NullQ = traceShow "solve false" False
      go (n, seenHits, cacheHits) seen cache ((_, st@(mfree@(MaxSet m free), cs)) :<! q)
        | sum (values cs) ≡ 0 = traceShow "solve true" True
        -- \| n > 100000 = traceShow "timeout" False
        | otherwise =
            let (cache', cacheHits') = case cache |? mfree of
                  Nothing -> (cache |. (mfree, fittingIFs vs mfree), cacheHits)
                  Just ifs -> (cache, cacheHits + 1)
                states =
                  -- nubOn key $
                  [ st'
                  | -- traceShow ("space", w ⋅ h - (g |.|), "size", ((head' (un vs)) |.|)) True,
                    -- mfree'@(MaxSet m' free') ← un (vars mfree),
                    (i, mfree') <- cache' |! mfree,
                    cs |? i > Just 0,
                    let cs' = cs |~ (i, subtract 1),
                    let st' = (mfree', cs')
                    -- traceTextLn (unlines $ (tshow ("next", st')) : (tshow <$> [toGridI free, toGrid v])) True
                    -- fits
                    -- key st' ∉ seen
                  ]
                k = key cache' st
                isSeen = k ∈ seen
                seenHits' = isSeen ??? seenHits + 1 $ seenHits
                seen' = k |-> seen
                q' = qAppend loss states q
             in traceShow
                  ( "ri",
                    ri,
                    "q",
                    size q,
                    "n",
                    n,
                    "cache",
                    size cache',
                    "seen",
                    size seen',
                    "seenhits",
                    (seenHits', showDP 4 $ fromIntegral seenHits' / fromIntegral n),
                    "cachehits",
                    (cacheHits', showDP 4 $ fromIntegral cacheHits' / fromIntegral n),
                    "free",
                    size free,
                    "cs",
                    [cs |! i | i <- [0 .. size cs - 1]]
                  )
                  $ if key cache' st ∈ seen
                    then go (n + 1, seenHits', cacheHits') seen cache' q
                    else go (n + 1, seenHits', cacheHits') seen' cache' q'
   in -- in traceVss vss $ go (0, 0) (∅) (mkQ₁ loss (b, (∅), cs))
      go (0, 0, 0) (∅) ((∅) @((MaxSet ℤ²) :|-> [(ℤ, MaxSet ℤ²)])) (mkQ₁ loss (mfree, cs))

fit' :: (ℤ -> ℤ² -> ℤ :|-> ℤ -> Vector (Set ℤ²) -> 𝔹) -> (ℤ, (ℤ², [ℤ])) -> 𝔹
fit' searchF (ri, ((w, h), ns)) =
  let cs :: ℤ :|-> ℤ = mkMap $ enum ns
      traceG g a = traceTextLn (pretty g) a
      vs :: Vector (Set ℤ²) = mk [mk (p |?> (#"#" □)) | p <- snd <$> ps]
   in searchF ri (w, h) cs vs

fitM :: (ℤ -> ℤ² -> ℤ :|-> ℤ -> Vector (MaxSet ℤ²) -> 𝔹) -> (ℤ, (ℤ², [ℤ])) -> 𝔹
fitM searchF (ri, ((w, h), ns)) =
  let cs :: ℤ :|-> ℤ = mkMap $ enum ns
      traceG g a = traceTextLn (pretty g) a
      vs :: Vector (MaxSet ℤ²) = mk [mk (p |?> (#"#" □)) | p <- snd <$> ps]
   in searchF ri (w, h) cs vs

search' :: ℤ -> ℤ² -> ℤ :|-> ℤ -> Vector (Set ℤ²) -> 𝔹
search' ri (w, h) cs vs =
  let go (mfree, cs)
        | sum (values cs) ≡ 0 = pure True
        | otherwise =
            let states =
                  [ (mfree', cs')
                  | (i, c) <- unMap cs,
                    c > 0,
                    let v' = vs !! i,
                    mfreeVar@(MaxSet m@(w, h) freeVar) <- vars mfree,
                    x <- [0 .. w - 3],
                    y <- [0 .. h - 3],
                    let v = setMap (bimap (+ x) (+ y)) v',
                    -- traceV mfreeVar v True,
                    v ∩ freeVar |=| v,
                    let mfree' = MaxSet m (freeVar ∖ v),
                    let cs' = cs |~ (i, subtract 1)
                  ]
             in or <$> go .=<<. states
      mfree@(MaxSet m free) = MaxSet (w - 1, h - 1) (box (0, 0) (w - 1, h - 1))
   in traceShow ri $ traceShowId $ run $ go .$. (mfree, cs)
  where
    toGrid :: MaxSet ℤ² -> Set ℤ² -> ".#X" ▦ ℤ²
    toGrid (MaxSet (w, h) free) v = mkGrid [(c, (c ∈ v ∧ c ∉ free) ??? (#"X" □) $ (c ∉ free ??? (#"#" □) $ (#"." □))) | x <- [0 .. w], y <- [0 .. h], let c = (x, y)]
    traceG g a = traceTextLn (pretty g) a
    traceV mfree v a = traceG (toGrid mfree v) a

search''' :: ℤ -> ℤ² -> ℤ :|-> ℤ -> Vector (Set ℤ²) -> 𝔹
search''' ri (w, h) cs vs =
  let go (mfree, cs)
        | sum (values cs) ≡ 0 = pure True
        | otherwise =
            let states =
                  [ (mfree', cs')
                  | (i, c) <- unMap cs,
                    c > 0,
                    let v' = vs !! i,
                    mfreeVar@(MaxSet m@(w, h) freeVar) <- vars mfree,
                    x <- [0 .. w - 3],
                    y <- [0 .. h - 3],
                    let v = setMap (bimap (+ x) (+ y)) v',
                    -- traceV mfreeVar v True,
                    v ∩ freeVar |=| v,
                    let mfree' = MaxSet m (freeVar ∖ v),
                    let cs' = cs |~ (i, subtract 1)
                  ]
             in or <$> go .=<<. states
      mfree@(MaxSet m free) = MaxSet (w - 1, h - 1) (box (0, 0) (w - 1, h - 1))
   in traceShow ri $ traceShowId $ run $ go .$. (mfree, cs)
  where
    toGrid :: MaxSet ℤ² -> Set ℤ² -> ".#X" ▦ ℤ²
    toGrid (MaxSet (w, h) free) v = mkGrid [(c, (c ∈ v ∧ c ∉ free) ??? (#"X" □) $ (c ∉ free ??? (#"#" □) $ (#"." □))) | x <- [0 .. w], y <- [0 .. h], let c = (x, y)]
    traceG g a = traceTextLn (pretty g) a
    traceV mfree v a = traceG (toGrid mfree v) a

contiguous ::
  forall f a {n}.
  ( MagnitudeF (f a) ~ Integer,
    Arbitrary f a,
    a ~ (n, n),
    Ord n,
    Num n,
    Magnitude (f a),
    Memberable a (f a)
  ) =>
  f a -> 𝔹
contiguous shape =
  let go seen (c :<| q)
        | c ∉ shape ∨ c ∈ seen = go seen q
        | otherwise = go (c |-> seen) (q >< mk (neighborsNoDiags c))
      go seen _ = seen |=| shape
   in go (∅) (mkSeq [arbitrary shape])

-- min0 :: MaxSet ℤ² -> MaxSet ℤ²
-- min0 (MaxSet m s) = let (minX, minY) = biminimum s in mk $ setMap (bimap (subtract minX) (subtract minY)) s

buildWith :: ℤ² -> Set (MaxSet ℤ²) -> MaxSet ℤ² -> Set (MaxSet ℤ²)
buildWith (w, h) shapes mv
  | shapes ≡ (∅) = mk (vars mv)
  | otherwise =
      mk
        [ ms'
        | ms'@(MaxSet (sw, sh) s') <- un shapes,
          mv'@(MaxSet (vw, vh) v') <- vars @MaxSet mv,
          msx <- [0 .. sw + 1],
          msy <- [0 .. sh + 1],
          mvx <- [0 .. vw + 1],
          mvy <- [0 .. vh + 1],
          let ms :: MaxSet ℤ² = setMap (bimap (+ msx) (+ msy)) ms',
          let mv :: MaxSet ℤ² = setMap (bimap (+ mvx) (+ mvy)) mv',
          let ms'@(MaxSet (w', h') _) = mv ∪ ms,
          let mvs@(MaxSet _ vs) = mv ∩ ms,
          vs ≡ (∅),
          w' < w,
          h' < h,
          contiguous ms'
          -- traceShow ((msx, msy), (mvx, mvy)) True,
          -- traceShow "shape" True,
          -- traceShow ms True,
          -- traceV ms True,
          -- traceShow "plus" True,
          -- traceShow mv True,
          -- traceV mv True,
          -- traceShow "intersect" True,
          -- traceShow mvs True,
          -- traceV mvs True,
          -- traceShow "union" True,
          -- traceShow ms' True,
          -- traceV ms' True
        ]

search'' :: ℤ -> ℤ² -> ℤ :|-> ℤ -> Vector (Set ℤ²) -> 𝔹
search'' ri (w, h) cs vs =
  let mvs :: Vector (MaxSet ℤ²) = mk ∘ un <$> vs
      go cs
        | sum (values cs) ≡ 0 =
            traceShow (ri, cs) $
              pure (∅)
        | otherwise = do
            shapes <-
              foldl1 (∪)
                <$> sequence
                  [ do
                      shapes <- go .$. cs'
                      pure (buildWith (w, h) shapes (mvs !! i))
                  | (i, c) <- sortOn snd $ unMap cs,
                    c > 0,
                    let cs' = cs |~ (i, subtract 1)
                  ]
            traceV (arbitrary shapes) $
              traceShow (ri, (w, h), cs, size shapes) $
                pure shapes
   in traceShowId $ (run $ go .$. cs) ≢ (∅)

varSigs p =
  -- traceShow p $
  -- traceShowId $
  nub
    [[(x1 - x0, y1 - y0) | ((x0, y0), (x1, y1)) <- pairs (sort $ un v)] | v <- un (vars p)]

traceSig sig a =
  case (placeSig (10, 10) (MaxSet (10, 10) (∅)) (4, 4) sig) of
    Just g -> traceV g a
    Nothing -> traceShow ("failed to place", sig) a

placeSig (w, h) (MaxSet m g) start sig =
  let go (x, y) sigs g'
        | x < 0 ∨ y < 0 ∨ x ≥ w ∨ y ≥ h ∨ (x, y) ∈ g = Nothing
        | otherwise = case sigs of
            ((dx, dy) : sigs) -> go (x + dx, y + dy) sigs ((x, y) |-> g')
            [] -> Just (mk $ un ((x, y) |-> g'))
   in go start sig g

placeV (w, h) g vSigs =
  catMaybes
    [ (,j,(x, y)) <$> placeSig (w, h) g (x, y) sig
    | -- traceShow "v" True,
      -- traceV v True,
      (j, sig) <- enum vSigs,
      -- traceShow "sig" True,
      -- traceSig sig True,
      x <- [0 .. w - 1],
      y <- [0 .. h - 1]
    ]

placeVs :: ℤ -> ℤ² -> ℤ :|-> ℤ -> Vector (MaxSet ℤ²) -> 𝔹
placeVs ri (w, h) cs mvs =
  let vSigss = varSigs <$> mvs
      loss (cs, g, placed) = (sum (values cs), sum [v ^ 2 | v <- values cs])
      key (cs, g, placed) = placed
      go _ NullQ = traceShow "solve false" False
      go seen ((_, st@(cs, g, placed)) :<! q)
        | sum (values cs) ≡ 0 =
            -- traceV g $
            traceShow "solve true" True
        | (seen |.|) > 1000 = traceShow "timeout" False
        | key st ∈ seen = go seen q
        | otherwise =
            let states =
                  [ st'
                  | (i, c) <- unMap cs,
                    c > 0,
                    let cs' = cs |~ (i, subtract 1),
                    let gjcs' = placeV (w, h) g (vSigss !! i),
                    -- traceShow ("placed", i, "for", size gs') True,
                    (g', j, c) <- gjcs',
                    let st' = (cs', g', (j, c) |-> placed)
                  ]
                seen' = key st |-> seen
                q' = qAppend loss states q
             in traceShow
                  ( "ri",
                    ri,
                    "wh",
                    (w, h),
                    "q",
                    size q,
                    "seen",
                    size seen,
                    "cs",
                    [cs |! i | i <- [0 .. size cs - 1]]
                  )
                  $ go seen' q'
   in go (∅) (mkQ₁ loss (cs, (MaxSet (0, 0) (∅)), (∅)))

searcho :: ℤ -> ℤ² -> ℤ :|-> ℤ -> Vector (Set ℤ²) -> 𝔹
searcho ri (w, h) cs vs =
  let mvs :: Vector (MaxSet ℤ²) = mk ∘ un <$> vs
      loss (cs, mshape@(MaxSet (w, h) shape)) = sum (values cs)
      -- key (cs, shape) = (cs, shape)
      key (cs, shape) = shape -- (cs, shape)
      go _ NullQ = traceShow "solve false" False
      go seen ((_, st@(cs, shape)) :<! q)
        | sum (values cs) ≡ 0 = traceShow "solve true" True
        | key st ∈ seen = go seen q
        | otherwise =
            let states =
                  [ st'
                  | (i, c) <- unMap cs,
                    c > 0,
                    let cs' = cs |~ (i, subtract 1),
                    let shapes = buildWith (w, h) (mkSet [shape]) (mvs !! i),
                    traceShow ("shape", i, "produced", size shapes) True,
                    shape' <- un shapes,
                    let st' = (cs', shape'),
                    key st' ∉ seen
                  ]
                seen' = key st |-> seen
                q' = qAppend loss states q
             in traceShow
                  ( "ri",
                    ri,
                    "q",
                    size q,
                    "seen",
                    size seen,
                    -- "shape",
                    -- shape,
                    "cs",
                    [cs |! i | i <- [0 .. size cs - 1]]
                  )
                  $ go seen' q'
   in go (∅) (mkQ₁ loss (cs, (∅)))

growRs :: [(ℤ², [ℤ])] -> [(ℤ², [ℤ])]
growRs rs =
  let mvs :: Vector (MaxSet ℤ²) = mk [mk (p |?> (#"#" □)) | p <- snd <$> ps]
      vSigss = varSigs <$> mvs
      loss rs (MaxSet (mx, my) s, cs)
        | rs ≡ [] = ((99999999, 99999999), (99999999, 99999999))
        | mx > maxX ∨ my > maxY = ((99999999, 99999999), (99999999, 99999999))
        -- \| otherwise = ((mx, my), minimum [(mx + 1 - w) ^ 2 + (my + 1 - h) ^ 2 | ((w, h), ns) <- rs]) -- , sum [max 0 (n - c) | (c, n) <- (zip cs ns)])
        | otherwise =
            ( (0, 0),
              minimum [if mx + 1 > w ∨ my + 1 > h then (9999999, 9999999) else ((max 0 (w - (mx + 1)) + max 0 (h - (my + 1))), sum [negate c | (c, n) <- (zip cs ns)]) | ((w, h), ns) <- rs]
            )
        where
          (maxX, maxY) = both (subtract 1) $ bimaximum (fst <$> rs)
      -- else (mx + my) -- mx + my + 2 + (mx + 1) ⋅ (my + 1))
      go :: [(ℤ², [ℤ])] -> Set ([ℤ], MaxSet ℤ²) -> ℤ² :|-> Set [ℤ] -> MinQ (ℤ², ℤ²) (MaxSet ℤ², [ℤ]) -> [(ℤ², [ℤ])]
      go rs seen rToCs NullQ = rs
      go rs seen rToCs ((l, (st@(g@(MaxSet (mx, my) s), cs))) :<! q)
        | l ≢ loss rs st = traceShow "loss changed" $ go rs seen rToCs (qInsert (loss rs) st q)
        | size rs ≡ 0 = traceShow "solve true" rs
        | mx > maxX ∨ my > maxY = traceShow "too big" $ go rs seen rToCs q
        | (cs, g) ∈ seen =
            -- traceShow ("seen hit", (mx, my), cs) $
            go rs seen rToCs q
        | otherwise =
            let (states, newCs) =
                  unzip
                    [ (st', ((w', h'), cs'))
                    | (i, vSigs) <- enum (un vSigss),
                      let cs' = cs !. (i, (cs !! i + 1)),
                      gv <- un (vars g),
                      -- (g'@(MaxSet (mx', my') s'), j, (x, y)) <- placeV (mx + 4, my + 4) g vSigs,
                      (gv', j, (x, y)) <- placeV (mx + 4, my + 4) gv vSigs,
                      contiguous gv',
                      -- g'@(MaxSet (mx', my') s') <- un (vars gv'),
                      let g'@(MaxSet (mx', my') s') = gv',
                      (mx' ≤ maxX ∧ my' ≤ maxY) ∨ (my' ≤ maxX ∧ mx' ≤ maxY),
                      (cs', g') ∉ seen,
                      let (w', h') = (mx' + 1, my' + 1),
                      let st' = (g', cs')
                      -- traceShow st' True
                      -- traceShow ("grew", g') True,
                      -- traceV g True,
                      -- traceV g' True
                    ]
                -- seen' = foldl' (\seen g -> (cs, g) |-> seen) seen (un (vars g))
                -- seen' = foldl' (\seen g -> (cs, g) |-> seen) seen (un (vars g))
                seen' = (cs, g) |-> seen
                rToCs' = foldl' (\rToCs (r, c) -> if r ∈ rToCs then rToCs |~ (r, (cs |->)) else rToCs |. (r, mk [cs])) rToCs newCs
                possible r@((w, h), ns) = or [and [c ≥ n | (c, n) <- zip cs' ns] | ((w', h'), cs') <- newCs, w' ≤ w, h' ≤ h]
                rs' = rs |-?-> (not ∘ possible)
                q' = qAppend (loss rs') states q
             in -- traceRToCs rToCs $
                traceShow ("rs", size rs, if size rs ≢ size rs' then "found" else "nope", size q, "seen", size seen, "mxy", (mx, my), "cs", cs) $
                  go rs' seen' rToCs' q'
        where
          (maxX, maxY) = both (subtract 1) $ bimaximum (fst <$> rs)
   in go rs (∅) (∅) (mkQ₁ (loss rs) ((MaxSet (0, 0) (∅)), (const 0 <$> ps)))

toG :: MaxSet ℤ² -> ".#X" ▦ ℤ²
toG (MaxSet (w, h) v) = mkGrid [(c, c ∈ v ??? (#"#" □) $ (#"." □)) | x <- [0 .. w], y <- [0 .. h], let c = (x, y)]

traceG g a = traceTextLn (pretty g) a

traceV v a = traceG (toG v) a

traceVs vs a = foldl' (\a v -> traceV v a) a vs

traceRToCs rToCs a =
  traceTextLn (unlines $ tshow <$> [(r, cs) | (r, css) <- sort $ unMap rToCs, cs <- un css]) a

-- part1 :: ℤ = enum rs |?| fitM placeVs
part1 :: ℤ = size rs - size (growRs rs)

--     size [r | ((w, h), ns) <- rs, [cs | ((w', h'), css) <- unMap rToCs, w' ≤ w, h' ≤ h, cs <- un css, and [c ≥ n | (c, n) <- zip cs ns]] ≢ []]

part2 :: ℤ = 0

vars :: forall f a. (Rotatable (f a), HMirrorable (f a), VMirrorable (f a)) => f a -> [f a]
vars xs = [f xs | f <- (∘) <$> [id, (↻), (↻) ∘ (↻), (↺)] <*> [id, (◓), (◐)]]

fittingIFs vs mfree =
  [ (i, mfree')
  | (i, v') <- enum (un vs),
    mfreeVar@(MaxSet m@(w, h) freeVar) <- vars mfree,
    x <- [0 .. w - 3],
    y <- [0 .. h - 3],
    let v = setMap (bimap (+ x) (+ y)) v',
    v ∩ freeVar |=| v,
    let mfree' = MaxSet m (freeVar ∖ v)
  ]
