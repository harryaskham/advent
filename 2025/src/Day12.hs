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

type Decomp = [ℤ] -> [(ℤ, [ℤ])]

decomp :: Decomp
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

decomp1 :: Decomp
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

part1 :: ℤ
part1 = (((possibleDecomposed decomper shaper placer shapess <$> taker rs) <>?) |.|)
  where
    shaper = shapesQ
    decomper ns = [(1, ns)]
    -- decomper = decomp
    -- decomper = decomp1
    placer = place
    -- placer = placeEdge
    taker = id

-- taker = id

part2 :: ℤ = 0

toG :: (Foldable f) => f ℤ² -> ".#X" ▦ ℤ²
toG xs =
  let (maxX, maxY) = bimaximum xs
      (minX, minY) = biminimum xs
      cs = mkSet (toList xs)
   in mkGrid [((x - minX, y - minY), (x, y) ∈ cs ??? (#"#" □) $ (#"." □)) | x <- [minY .. maxX], y <- [minY .. maxY]]

traceV v a = traceGrid (toG v) a

shapeWH' = shapeWH ∘ fst

type Shaper = Placer -> [[Shape ℤ²]] -> ([ℤ] .->. [Shape ℤ²])

shapesQ :: Shaper
shapesQ placer shape1ss ns' =
  let go :: [ℤ] .->. MinQ ℤ² (Shape ℤ²)
      go ns
        | any (< 0) ns = pure (∅)
        | sum ns ≡ 0 = pure (mkQ₁ shapeWH (∅))
        | otherwise = do
            ishape0ss <- sequence [((i,) <$> (go .$. (ns !. (i, (n - 1))))) | (i, n) <- enum ns]
            let f i shape01s shape0 = case placer shape0 (shape1ss !! i) of
                  shape01sL@(s : _) -> if ns ≡ ns' then mkQ₁ shapeWH s else qAppend shapeWH shape01sL shape01s
                  [] -> shape01s
            let g shape01s (i, shape0s) = case foldlU' (f i) shape01s shape0s of
                  q@NullQ -> q
                  q@((_, s) :<! _) -> if ns ≡ ns' then mkQ₁ shapeWH s else q
            let shape01s = case foldl' g (∅) ishape0ss of
                  q@NullQ -> q
                  q@((_, s) :<! _) -> traceShow ns ∘ traceShape s $ if ns ≡ ns' then mkQ₁ shapeWH s else q
            pure $ case shape01s of
              q@NullQ -> q
              q@((_, s) :<! _) -> if ns ≡ ns' then (mkQ₁ shapeWH s) else q
   in toList ∘ go

shapesL :: Shaper
shapesL placer shape1ss =
  let go :: [ℤ] .->. [Shape ℤ²]
      go ns
        | any (< 0) ns = pure []
        | sum ns ≡ 0 = pure [(∅)]
        | otherwise = do
            ishape0ss <- sequence [((i,) <$> (go .$. (ns !. (i, (n - 1))))) | (i, n) <- enum ns]
            let f i shape01s shape0 = placer shape0 (shape1ss !! i)
            let g shape01s (i, shape0s) = foldl' (f i) shape01s shape0s
            let shape01s = foldl' g [] ishape0ss
            pure shape01s
   in go

possibleDecomposed :: Decomp -> Shaper -> Placer -> [[Shape ℤ²]] -> (ℤ², [ℤ]) -> Maybe (Shape ℤ²)
possibleDecomposed decomp shaper placer shapess r@((w, h), ns) =
  traceShow ("possibleDecomposed", ns) $
    let (shapess', ns') =
          run (unzip <$> traverse (\(n', ns') -> (toList <$> (shaper placer shapess .$. ns')) <&> (,n')) (decomp ns))
     in traceShow (ns, "decomped to", ns', size <$> shapess') $
          traceN traceShape (head' shapess') $
            possible shaper placer shapess' ((w, h), ns')

possible :: Shaper -> Placer -> [[Shape ℤ²]] -> (ℤ², [ℤ]) -> Maybe (Shape ℤ²)
possible shaper placer shapess r@((w, h), ns) =
  traceShow ("possible", ns) $ run do
    shapess' <- toList <$> (shaper placer shapess .$. ns)
    pure $ case [shape | shape <- shapess', validShape (w, h) shape] of
      [] -> traceShow "no fit" $ Nothing
      (shape : _) -> traceShow "fit" ∘ traceShape shape $ Just shape

type Placer = Shape ℤ² -> [Shape ℤ²] -> [Shape ℤ²]

placeEdge :: Placer
placeEdge EmptyShape shapes = shapes
-- \|-?-> validShape (w, h)
placeEdge Invalid shapes = []
placeEdge shape0' shapes =
  [ -- traceShow ("made", (w, h), shapeWH shape01) $
  shape01
  | let shape0@(Shape cs0 s0 ds0 ((lx0, ly0), (ux0, uy0))) = alignShape shape0',
    let (w0, h0) = shapeWH shape0,
    shape1' <- (alignShape <$> shapes),
    -- \|-?-> validShape (w, h),
    -- traceShape shape1' True,
    let (w1, h1) = shapeWH shape1',
    xO <- range (0 - w1) 0 <> range (w1 - w0) w1,
    yO <- range (0 - h1) 0 <> range (h1 - h0) h1,
    let shape1 = offsetShape xO yO shape1',
    -- traceShape shape1 True,
    let shape01 = alignShape $ shape0 <> shape1
    -- validShape (w, h) shape01
    -- traceShape shape01 True,
  ]
placeEdge _ shapes = []

place :: Placer
place EmptyShape shapes = shapes
-- \|-?-> validShape (w, h)
place Invalid shapes = []
place shape0' shapes =
  [ -- traceShow ("made", (w, h), shapeWH shape01) $
  shape01
  | let shape0@(Shape cs0 s0 ds0 ((lx0, ly0), (ux0, uy0))) = alignShape shape0',
    let (w0, h0) = shapeWH shape0,
    shape1' <- (alignShape <$> shapes),
    -- \|-?-> validShape (w, h),
    -- traceShape shape1' True,
    let (w1, h1) = shapeWH shape1',
    xO <- range (negate w1) w0,
    yO <- range (negate h1) h0,
    let shape1 = offsetShape xO yO shape1',
    -- traceShape shape1 True,
    let shape01 = alignShape $ shape0 <> shape1
    -- validShape (w, h) shape01
    -- traceShape shape01 True,
  ]
place _ shapes = []
