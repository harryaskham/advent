module Day12 where

(ps, rs) :: [(ℤ, ".#" ▦ ℤ²)] × [(ℤ², [ℤ])] =
  $(aocx 12)
    -- \$(aoc 12)
    -- \$(aocxn 12 1)
    & (⊏|⊐) @(([(ℤ, ".#" ▦ ℤ²) ⯻ ":\n"] ≠ []) × ([(ℤ² ⯻ "x", [ℤ] ⯻ " ") ⯻ ": "] ≠ []))

vars :: forall f a. (Eq (f a), Rotatable (f a), HMirrorable (f a), VMirrorable (f a)) => f a -> [f a]
vars xs = nub [f xs | f <- (∘) <$> [id, (↻), (↻) ∘ (↻), (↺)] <*> [id, (◓), (◐)]]

vss :: [[MaxSet ℤ²]] = [vars (mk (p |?> (#"#" □))) | p <- snd <$> ps]

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

data Shape a where
  EmptyShape :: Shape a
  Invalid :: Shape a
  Shape :: (Ord a, MkDiffList a) => Seq a -> Set a -> DiffList a -> (a, a) -> Shape a

deriving instance (Show a) => Show (Shape a)

deriving instance (Eq a) => Eq (Shape a)

deriving instance (Ord a) => Ord (Shape a)

getCs (Shape cs _ _ _) = cs

getS (Shape _ s _ _) = s

getDs (Shape _ _ ds _) = ds

getBs (Shape _ _ _ bs) = bs

type instance Element (Shape a) = a

instance Foldable Shape where
  foldr _ accum Invalid = accum
  foldr _ accum EmptyShape = accum
  foldr f accum (Shape _ s _ _) = foldr f accum s

mkShape :: (Foldable f) => f ℤ² -> Shape ℤ²
mkShape cs = case toList cs of
  [] -> EmptyShape
  cs ->
    let s = mkSet cs
        lb = biminimum cs
        ub = bimaximum cs
     in if s |≢| cs
          then Invalid
          else Shape (mk cs) s (mkDiffList cs) (lb, ub)

instance (Semigroup (Shape a)) => Monoid (Shape a) where
  mempty = EmptyShape

instance (Bimaximum a, Biminimum a, MkDiffList a) => Semigroup (Shape a) where
  Invalid <> s = Invalid
  s <> Invalid = Invalid
  EmptyShape <> s = s
  s <> EmptyShape = s
  (Shape cs0@(_ :|> l0) s0 ds0 (lb0, ub0)) <> (Shape cs1@(h1 :<| _) s1 ds1 (lb1, ub1)) =
    let s01 = s0 ∪ s1
     in ((s01 |.|) ≡ (s0 |.|) + (s1 |.|))
          ??? (Shape (cs0 >< cs1) s01 (diffListConcatVia ds0 l0 h1 ds1) (biminimum [lb0, lb1], bimaximum [ub0, ub1]))
          $ Invalid

offsetShape x y Invalid = Invalid
offsetShape x y EmptyShape = EmptyShape
offsetShape x y (Shape cs s ds ((lx, ly), (ux, uy))) =
  let cs' = bimap (+ x) (+ y) <$> cs
   in Shape cs' (mk $ un cs') ds ((lx + x, ly + y), (ux + x, uy + y))

shapess :: [[Shape ℤ²]] = [[mkShape v | v <- vars (mk @MaxSet (p |?> (#"#" □)))] | p <- snd <$> ps]

validShape _ Invalid = False
validShape _ EmptyShape = True
validShape wh shape = boundedShape wh shape

boundedShape _ Invalid = False
boundedShape _ EmptyShape = True
boundedShape (w, h) shape = let (sw, sh) = shapeWH shape in sw ≤ w ∧ sh ≤ h

shapeWH Invalid = (0, 0)
shapeWH EmptyShape = (0, 0)
shapeWH (Shape _ _ _ ((lx, ly), (ux, uy))) = (ux - lx + 1, uy - ly + 1)

alignShape Invalid = Invalid
alignShape EmptyShape = EmptyShape
alignShape shape@(Shape _ _ _ ((lx, ly), (ux, uy))) = offsetShape (negate lx) (negate ly) shape

place :: ℤ² -> Shape ℤ² -> [Shape ℤ²] -> [Shape ℤ²]
place (w, h) EmptyShape shapes = shapes |-?-> validShape (w, h)
place (w, h) Invalid shapes = []
place (w, h) shape0' shapes =
  [ shape01
  | let shape0@(Shape cs0 s0 ds0 ((lx0, ly0), (ux0, uy0))) = alignShape shape0',
    let (w0, h0) = shapeWH shape0,
    shape1' <- (alignShape <$> shapes) |-?-> validShape (w, h),
    -- traceShape shape1' True,
    let (w1, h1) = shapeWH shape1',
    xO <- range (negate w1) w0,
    yO <- range (negate h1) h0,
    let shape1 = offsetShape xO yO shape1',
    -- traceShape shape1 True,
    let shape01 = alignShape $ shape0 <> shape1,
    validShape (w, h) shape01
    -- traceShape shape01 True,
  ]
place _ _ shapes = []

shapes :: ℤ² -> [[Shape ℤ²]] -> ([ℤ] .->. MinQ ℤ² (Shape ℤ²))
shapes (w, h) shape1ss =
  let go :: [ℤ] .->. MinQ ℤ² (Shape ℤ²)
      go ns
        | any (< 0) ns = pure (∅)
        | sum ns ≡ 0 = pure (mkQ₁ shapeWH (∅))
        | otherwise = do
            ishape0ss <- sequence [((i,) <$> (go .$. (ns !. (i, (n - 1))))) | (i, n) <- enum ns]
            let f i shape01s shape0 =
                  let shape01sL = place (w, h) shape0 (shape1ss !! i)
                   in -- (traceShow ns ∘ traceShape (arbitrary shape01sL)) $
                      qAppend shapeWH shape01sL shape01s
            let g shape01s (i, shape0s) = foldlU' (f i) shape01s shape0s
            let shapes = foldl' g (∅) ishape0ss
            let tf =
                  case shapes of
                    NullQ -> traceShow (ns, "no shapes")
                    _ -> traceShow ns . traceV (arbitrary shapes)
            pure $ tf shapes
   in go

shapesL :: ℤ² -> [[Shape ℤ²]] -> ([ℤ] .->. [Shape ℤ²])
shapesL (w, h) shape1ss =
  let go :: [ℤ] .->. [Shape ℤ²]
      go ns
        | any (< 0) ns = pure []
        | sum ns ≡ 0 = pure [(∅)]
        | otherwise = do
            -- TODO: here we acn decomp by halves
            ishape0ss <- sequence [((i,) <$> (go .$. (ns !. (i, (n - 1))))) | (i, n) <- enum ns]
            let f i shape01s shape0 =
                  let shape01sL = place (w, h) shape0 (shape1ss !! i)
                   in -- (traceShow ns ∘ traceShape (arbitrary shape01sL)) $
                      shape01s <> shape01sL
            let g shape01s (i, shape0s) = foldl' (f i) shape01s shape0s
            let shapes = foldl' g [] ishape0ss
            let tf =
                  case shapes of
                    [] -> traceShow (ns, "no shapes")
                    _ -> traceShow ns . traceV (arbitrary shapes)
            pure $ tf shapes
   in go

area :: Shape ℤ² -> ℤ
area shape = (*) $@ shapeWH shape

showShape :: Shape ℤ² -> Text
showShape Invalid = "invalid"
showShape EmptyShape = "empty"
showShape shape@(Shape cs _ _ bs) =
  unlines
    [ tshow (size cs, bs),
      pretty (toG shape)
    ]

showShapes :: [Shape ℤ²] -> Text
showShapes = unlines ∘ fmap showShape

showShapess :: [[Shape ℤ²]] -> Text
showShapess = unlines ∘ fmap showShapes

traceShape s a = traceTextLn (showShape s) a

decomp :: [ℤ] -> [(ℤ, [ℤ])]
decomp ns = go [(1, ns)]
  where
    go :: [(ℤ, [ℤ])] -> [(ℤ, [ℤ])]
    go cns = iterateFix (go1 =<<) cns
    go1 :: (ℤ, [ℤ]) -> [(ℤ, [ℤ])]
    go1 (c, ns)
      | all (≡ 0) ns = []
      | all (< 2) ns = [(c, ns)]
      | otherwise =
          tracePrefixId ("decomp", ns) $
            let (qs, rs) = unzip [n `quotRem` 2 | n <- ns]
             in (2 ⋅ c, qs) : go1 (c, rs)

decomp1 :: [ℤ] -> [(ℤ, [ℤ])]
decomp1 ns = swap <$> unMap cs
  where
    nss :: [[ℤ]] = go1 ns
    cs :: Map [ℤ] ℤ = counts @[Integer] @Integer nss
    go1 :: [ℤ] -> [[ℤ]]
    go1 ns
      | all (≡ 0) ns = []
      | otherwise =
          let ns' = [n > 0 ??? 1 $ 0 | n <- ns]
              ns'' = [n - n' | (n, n') <- zip ns ns']
           in tracePrefixId ("decomp", ns) $
                ns' : go1 ns''

possible :: [[Shape ℤ²]] -> (ℤ², [ℤ]) -> Maybe (Shape ℤ²)
possible shapess r@((w, h), ns) =
  let (shapess', ns') = run $ unzip <$> traverse (\(n', ns') -> (shapes (w, h) shapess .$. ns') <&> (,n')) (tracePrefixId "decomp" (decomp ns))
      shapessL' = nub ∘ toList <$> shapess'
   in traceShow (size <$> shapessL', "intermediary", ns') $ possible' shapessL' ((w, h), ns')

possibleL :: [[Shape ℤ²]] -> (ℤ², [ℤ]) -> Maybe (Shape ℤ²)
possibleL shapess r@((w, h), ns) =
  let (shapess', ns') = run $ unzip <$> traverse (\(n', ns') -> (shapesL (w, h) shapess .$. ns') <&> (,n')) (tracePrefixId "decomp" (decomp ns))
   in traceShow (size <$> shapess', "intermediary", ns') $ possibleL' shapess' ((w, h), ns')

possibleL' :: [[Shape ℤ²]] -> (ℤ², [ℤ]) -> Maybe (Shape ℤ²)
possibleL' shapess r@((w, h), ns) =
  traceShow ("possible'", ns) $
    let shapess' = run $ dropWhile (\s -> let (sw, sh) = shapeWH s in sw > w ∨ sh > h) <$> shapesL (w, h) shapess .$. ns
     in case shapess' of
          [] -> traceShow "no fit" $ Nothing
          ((shape) : _) -> traceShow "arb fit" ∘ traceShape shape $ Just shape

possible' :: [[Shape ℤ²]] -> (ℤ², [ℤ]) -> Maybe (Shape ℤ²)
possible' shapess r@((w, h), ns) =
  traceShow ("possible'", ns) $
    let q = run $ shapes (w, h) shapess ns
     in traceShow
          (size q, "fits")
          $ case takeWhileWithKey (\(w', h') _ -> w' ≤ w ∧ h' ≤ h) q of
            [] -> traceShow "no fit" $ Nothing
            ((_, shape) : _) -> traceShow "arb fit" ∘ traceShape shape $ Just shape

-- part1 :: ℤ = (((possible shapess <$> rs) <>?) |.|)
part1 :: ℤ = (((solveR <$> rs) <>?) |.|)

solveR r = possible1' shapess r

part2 :: ℤ = 0

toG :: (Foldable f) => f ℤ² -> ".#X" ▦ ℤ²
toG xs =
  let (maxX, maxY) = bimaximum xs
      (minX, minY) = biminimum xs
      cs = mkSet (toList xs)
   in mkGrid [((x - minX, y - minY), (x, y) ∈ cs ??? (#"#" □) $ (#"." □)) | x <- [minY .. maxX], y <- [minY .. maxY]]

traceV v a = traceGrid (toG v) a

decompR :: ℤ² -> [ℤ²]
decompR (w, h) =
  let go (w, h)
        | w < 4 ∨ h < 4 = [(w, h)]
        | otherwise =
            let (w', wq) = w `quotRem` 2
                (h', hq) = h `quotRem` 2
                cs = [(w', h'), (w' + wq, h' + hq)]
             in go =<< (cs <> cs)
   in go (w, h)

possibleR :: [[Shape ℤ²]] -> (ℤ², [ℤ]) -> [MinQ ℤ² (Shape ℤ², [ℤ])]
possibleR shapess r@((w, h), ns) =
  let dWHs = decompR (w, h)
      cWHs = counts dWHs
      whs = values cWHs
      rShapes (w, h) = shapesB (w, h) shapess ns
      cShapesNs = run $ traverse rShapes whs
   in cShapesNs

shapeWH' = shapeWH ∘ fst

shapesB :: ℤ² -> [[Shape ℤ²]] -> ([ℤ] .->. MinQ ℤ² (Shape ℤ², [ℤ]))
shapesB (w, h) shape1ss ns' =
  let go :: [ℤ] .->. MinQ ℤ² (Shape ℤ², [ℤ])
      go ns
        -- \| any (< 0) ns = pure (mkQ₁ shapeWH' ((∅), const 0 <$> ns))
        | any (< 0) ns = pure (∅)
        | sum ns ≡ 0 = pure (mkQ₁ shapeWH' ((∅), ns'))
        | otherwise = do
            ishape0ss <- sequence [((i,) <$> (go .$. (ns !. (i, (n - 1))))) | (i, n) <- enum ns]
            let f i shape01s (shape0, ns0) =
                  let shape01sL = (,ns0 !. (i, (ns0 !! i) + 1)) <$> place (w, h) shape0 (shape1ss !! i)
                   in -- (traceShow ns ∘ traceShape (arbitrary shape01sL)) $
                      qAppend shapeWH' shape01sL shape01s
            let g shape01s (i, shape0s) = foldlU' (f i) shape01s shape0s
            let shapes = foldl' g (mconcat (snd <$> ishape0ss)) ishape0ss
            let tf =
                  case shapes of
                    NullQ -> traceShow (ns, "no shapes")
                    _ -> traceShow ns . traceV (fst $ arbitrary shapes)
            pure $ tf shapes
   in go ns'

-- slideR :: [[Shape ℤ²]] -> (ℤ², [ℤ]) -> [MinQ ℤ² (Shape ℤ², [ℤ])]
-- slideR shapess r@((w, h), ns)
--   | w < 4 ∨ h < 4 = shapesB (w, h) shapess ns
--   | otherwise =

shapesEdge :: (ℤ², [ℤ]) -> [[Shape ℤ²]] -> ([ℤ] .->. MinQ ℤ² (Shape ℤ²))
shapesEdge ((w, h), ns') shape1ss =
  let go :: [ℤ] .->. MinQ ℤ² (Shape ℤ²)
      go ns
        | any (< 0) ns = pure (∅)
        | sum ns ≡ 0 = pure (mkQ₁ shapeWH (∅))
        | otherwise = do
            ishape0ss <- sequence [((i,) <$> (go .$. (ns !. (i, (n - 1))))) | (i, n) <- enum ns]
            let f i shape01s shape0 =
                  let shape01sL = placeEdge (w, h) shape0 (shape1ss !! i)
                   in case (shape01sL, ns ≡ ns') of
                        ((s : _), True) -> mkQ₁ shapeWH s
                        _ -> qAppend shapeWH shape01sL shape01s
            let g shape01s (i, shape0s) = case foldlU' (f i) shape01s shape0s of
                  q@((_, s) :<! _) -> if ns ≡ ns' then mkQ₁ shapeWH s else q
                  q@NullQ -> q
            let shape01s = case foldl' g (∅) ishape0ss of
                  q@((_, s) :<! _) -> if ns ≡ ns' then mkQ₁ shapeWH s else q
                  q@NullQ -> q
            case shape01s of
              NullQ -> traceShow (ns, "no shapes") $ pure shape01s
              ((_, s) :<! _) -> traceShow (ns, "fit") $ pure (if ns ≡ ns' then mkQ₁ shapeWH s else shape01s)
   in go

placeEdge :: ℤ² -> Shape ℤ² -> [Shape ℤ²] -> [Shape ℤ²]
placeEdge (w, h) EmptyShape shapes = shapes |-?-> validShape (w, h)
placeEdge (w, h) Invalid shapes = []
placeEdge (w, h) shape0' shapes =
  [ shape01
  | let shape0@(Shape cs0 s0 ds0 ((lx0, ly0), (ux0, uy0))) = alignShape shape0',
    let (w0, h0) = shapeWH shape0,
    shape1' <- (alignShape <$> shapes) |-?-> validShape (w, h),
    -- traceShape shape1' True,
    let (w1, h1) = shapeWH shape1',
    xO <- range (0 - w1) 0 <> range (w1 - w0) w1,
    yO <- range (0 - h1) 0 <> range (h1 - h0) h1,
    let shape1 = offsetShape xO yO shape1',
    -- traceShape shape1 True,
    let shape01 = alignShape $ shape0 <> shape1,
    validShape (w, h) shape01
    -- traceShape shape01 True,
  ]
placeEdge _ _ shapes = []

possible1 :: [[Shape ℤ²]] -> (ℤ², [ℤ]) -> Maybe (Shape ℤ²)
possible1 shapess r@((w, h), ns) =
  let (shapess', ns') = run $ unzip <$> traverse (\(n', ns') -> (shapesEdge r shapess .$. ns') <&> (,n')) (tracePrefixId "decomp" (decomp1 ns))
      shapessL' = nub ∘ toList <$> shapess'
   in traceShow (size <$> shapessL', "intermediary", ns') $ possible1' shapessL' ((w, h), ns')

possible1' :: [[Shape ℤ²]] -> (ℤ², [ℤ]) -> Maybe (Shape ℤ²)
possible1' shapess r@((w, h), ns) =
  traceShow ("possible'", ns) $
    case run $ shapesEdge r shapess ns of
      NullQ -> traceShow "no fit" $ Nothing
      ((_, smallestShape) :<! q) ->
        traceShow "smallest" ∘ traceShape smallestShape $
          let (w', h') = shapeWH smallestShape
           in if w' ≤ w ∧ h' ≤ h
                then traceShow "fit" $ Just smallestShape
                else traceShow "smallest no fit" $ Nothing
