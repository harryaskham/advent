module Day12 where

(ps, rs) :: [(ℤ, ".#" ▦ ℤ²)] × [(ℤ², [ℤ])] =
  $(aocx 12)
    -- \$(aoc 12)
    -- \$(aocxn 12 1)
    & (⊏|⊐) @(([(ℤ, ".#" ▦ ℤ²) ⯻ ":\n"] ≠ []) × ([(ℤ² ⯻ "x", [ℤ] ⯻ " ") ⯻ ": "] ≠ []))

type C f a =
  ( ShapeLike a,
    Monoid (f a),
    Foldable f,
    Alternative f,
    Mkable f ℤ,
    Semigroup (f ℤ²),
    Semigroup (f ℤ),
    Mkable f a,
    Sizable (f ℤ²),
    Arbitrary f ℤ²,
    Takeable ℤ f a,
    Mkable f (ℤ, a)
  ) ::
    Constraint

vars :: forall g f a. (Mkable g (f a), Foldable f, Eq (f a), Rotatable (f a), HMirrorable (f a), VMirrorable (f a)) => f a -> g (f a)
vars xs = mk [f xs | f <- (∘) <$> [id, (↻), (↻) ∘ (↻), (↺)] <*> [id, (◓), (◐)]]

vss :: [[MaxSet ℤ²]] = [vars (mk (p |?> (#"#" □))) | p <- snd <$> ps]

contiguous ::
  forall f a {n}.
  ( MagnitudeF (f a) ~ Integer,
    ShapeLike a,
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

contiguousShape :: a -> 𝔹
contiguousShape Invalid = False
contiguousShape EmptyShape = True
contiguousShape (Shape cs _ _ _) = contiguous (un cs)

data Shape a where
  EmptyShape :: Shape a
  Invalid :: Shape a
  Shape :: (Ord a, MkDiffList a) => Seq a -> Set a -> DiffList a -> (a, a) -> Shape a

deriving instance (Show a) => Show (Shape a)

deriving instance (Eq a) => Eq (Shape a)

deriving instance (Ord a) => Ord (Shape a)

instance Sizable (Shape a) where
  size Invalid = 0
  size EmptyShape = 0
  size (Shape cs _ _ _) = size cs

instance (Num a, Ord a) => Rotatable (Shape (a, a)) where
  (↺) EmptyShape = EmptyShape
  (↺) Invalid = Invalid
  (↺) (Shape cs s ds bs) = mkShape ((mk @MaxSet (un cs)) ↺)

  (↻) EmptyShape = EmptyShape
  (↻) Invalid = Invalid
  (↻) (Shape cs s ds bs) = mkShape ((mk @MaxSet (un cs)) ↻)

instance (Num a, Ord a) => HMirrorable (Shape (a, a)) where
  (◐) EmptyShape = EmptyShape
  (◐) Invalid = Invalid
  (◐) (Shape cs s ds bs) = mkShape ((mk @MaxSet (un cs)) ◐)

instance (Num a, Ord a) => VMirrorable (Shape (a, a)) where
  (◓) EmptyShape = EmptyShape
  (◓) Invalid = Invalid
  (◓) (Shape cs s ds bs) = mkShape ((mk @MaxSet (un cs)) ◓)

getCs (Shape cs _ _ _) = cs

getS (Shape _ s _ _) = s

getDs (Shape _ _ ds _) = ds

getBs (Shape _ _ _ bs) = bs

type instance Element (Shape a) = a

instance Foldable Shape where
  foldr _ accum Invalid = accum
  foldr _ accum EmptyShape = accum
  foldr f accum (Shape _ s _ _) = foldr f accum s

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

-- shapess :: [[Shape ℤ²]] = [[mkShape v | v <- vars (mk @MaxSet (p |?> (#"#" □)))] | p <- snd <$> ps]

shapess :: forall f a. (Mkable f a, ShapeLike a) => [f a]
shapess = [mk [mkShape @a (mk @MaxSet (p |?> (#"#" □)))] | p <- snd <$> ps]

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

part2 :: ℤ = 0

trN ::
  ( Foldable f,
    Integral i,
    Takeable i f a,
    Mkable f (ℤ, a),
    Show l
  ) =>
  l -> i -> f a -> a -> a
trN label n xs a =
  foldl' (\a (i, x) -> traceShow (label, i) ∘ traceShape x $ a) a (enum (take n xs))

trNM ::
  ( Takeable Int g (f a),
    Takeable Int f a,
    Mkable g (ℤ, f a),
    Mkable f (ℤ, a),
    Foldable f,
    Foldable g,
    Show l
  ) =>
  l -> Int -> Int -> g (f a) -> a -> a
trNM label n m xss a =
  ( ( Ł
        ( \a (i, xs) ->
            ((Ł (\a (j, x) -> traceShow (label, i, j) ∘ traceShape x $ a) a (enum (take m xs))) !>)
        )
        a
        (enum (take n xss))
    )
      !>
  )

trNId ::
  ( Integral i,
    Foldable f,
    Takeable i f a,
    Mkable f (ℤ, a),
    Show l
  ) =>
  l -> i -> f a -> f a
trNId label n xs = trN label n xs xs

toG :: (Foldable f) => f ℤ² -> ".#X" ▦ ℤ²
toG xs =
  let (maxX, maxY) = bimaximum xs
      (minX, minY) = biminimum xs
      cs = mkSet (toList xs)
   in mkGrid [((x - minX, y - minY), (x, y) ∈ cs ??? (#"#" □) $ (#"." □)) | x <- [minY .. maxX], y <- [minY .. maxY]]

traceV v a = traceGrid (toG v) a

shapeWH' = shapeWH ∘ fst

class Possible f a where
  possibleDecomposed :: [f a] -> (ℤ², [ℤ]) -> Maybe a
  default possibleDecomposed :: (C f a) => [f a] -> (ℤ², [ℤ]) -> Maybe a
  possibleDecomposed shapess r@((w, h), ns) =
    trNM ("possibleDecomposed", ns) 6 3 shapess $
      let (shapess', ns') =
            run (unzip <$> traverse (\(n', ns') -> (toList <$> (shapes (w, h) shapess .$. ns')) <&> (,n')) (decomp ns))
          res = possible @f shapess' ((w, h), ns')
       in traceShow (ns, "decomped to", ns', size <$> shapess') $
            foldl'
              ( \a ss -> case arb ss of
                  Just s -> traceShape s a
                  Nothing -> traceShow "intermediate failed" a
              )
              res
              shapess'

  possible :: (C f a) => [f a] -> (ℤ², [ℤ]) -> Maybe a
  default possible :: (Foldable f, Arbitrary f ℤ²) => [f a] -> (ℤ², [ℤ]) -> Maybe a
  possible shapess r@((w, h), ns) =
    trNM ("possible", ns) 6 3 shapess $ run do
      shapess' <- shapes (w, h) shapess .$. ns
      pure $ case arb [shape | shape <- toList shapess', validShape (w, h) shape] of
        Nothing -> traceShow "no fit" $ Nothing
        Just shape -> traceShow "fit" ∘ traceShape shape $ Just shape

instance (C f a) => Possible f a

class PlaceRange f a where
  rangeEdge :: ℤ² -> ℤ² -> f ℤ²
  default rangeEdge :: (C f a) => ℤ² -> ℤ² -> f ℤ²
  rangeEdge (w0, h0) (w1, h1) =
    [(xO, yO) | xO <- ((0 - w1 - 1) |...| 0) <> ((w1 - w0) |...| (w1 + 1)), yO <- (0 - h1 - 1) |...| (h1 + 1)]
      <> [(xO, yO) | xO <- (0 - w1 - 1) |...| w0, yO <- ((0 - h1 - 1) |...| 0) <> ((h0 - h1 - 1) |...| (h1 + 1))]

  rangeBlock :: ℤ² -> ℤ² -> f ℤ²
  default rangeBlock :: (C f a) => ℤ² -> ℤ² -> f ℤ²
  rangeBlock (w0, h0) (w1, h1) =
    [ (xO, yO)
    | xO <- (0 - w1 - 1) |...| (w0 + 1),
      yO <- (0 - h1 - 1) |...| (h0 + 1)
    ]

instance (C f a) => PlaceRange f a

class Place f a where
  place :: a -> f a -> f a

instance (C f a) => Place f (Shape ℤ²) where
  place shape0U shape1s
    | shape0U ≡ (∅) = [shape | shapeO <- shape1s, shape <- vars shapeO, validShape' shape]
    | not (validShape' shape0U) = mempty
    | otherwise =
        traceShow "place" $
          [ shape01
          | -- traceShow "shape0 unaligned" True,
            -- traceShape shape0U True,
            let shape0@(Shape cs0 s0 ds0 ((lx0, ly0), (ux0, uy0))) = alignShape shape0U,
            -- traceShow "shape0 aligned" True,
            -- traceShape shape0 True,
            let (w0, h0) = shapeWH shape0,
            shape1UO <- shape1s,
            -- traceShow "shape1 unaligned, unvaried" True,
            -- traceShape shape1UO True,
            validShape' shape1UO,
            shape1U <- vars shape1UO,
            -- traceShow "shape1 unaligned, varied" True,
            -- traceShape shape1U True,
            let shape1 = alignShape shape1U,
            -- traceShow "shape1 aligned, varied" True,
            -- traceShape shape1 True,
            let (w1, h1) = shapeWH shape1,
            (xO, yO) <- rangeBlock (w0, h0) (w1, h1),
            let shape1Offset = offsetShape xO yO shape1,
            -- traceShow "shape1 aligned, varied, offset" True,
            -- traceShape shape1Offset True,
            let shape01U = shape0 <> shape1Offset,
            -- traceShow "shape01 unaligned" True,
            -- traceShape shape01U True,
            validShape' shape01U,
            let shape01 = alignShape shape01U,
            let filterValid = contiguousShape shape01,
            -- traceShow ("shape01 filter ok", filterValid) True,
            filterValid,
            traceShow "shape01 aligned" True,
            traceShape shape01 True
          ]

instance (Place f (Shape ℤ²)) => Place f LossShape where
  place (LossShape shape0U) shape1s = LossShape <$> (place shape0U (unLossShape <$> shape1s))

class Shapes f a where
  shapes :: ℤ² -> [f a] -> ([ℤ] .->. f a)
  default shapes :: (C f a) => ℤ² -> [f a] -> ([ℤ] .->. f a)
  shapes (w, h) shape1ss =
    let go :: [ℤ] .->. f a
        go ns
          | any (< 0) ns = pure mempty
          | sum ns ≡ 0 = pure $ pure (∅)
          | otherwise = do
              ishape0ss <- sequence [((i,) <$> (go .$. (ns !. (i, (n - 1))))) | (i, n) <- enum ns]
              let f i shape01s shape0 = [shape | shape <- place shape0 (shape1ss !! i), validShape (w, h) shape]
              let g shape01s (i, shape0s) = ((Ł (f i) shape01s shape0s) !>)
              let shape01s = ((Ł g (∅) ishape0ss) !>)
              pure $ trNId ("shapes", ns) 5 shape01s
     in go

instance (C f a) => Shapes f a

class ShapeLike a where
  mkShape :: (Foldable f) => f ℤ² -> a
  validShape :: ℤ² -> a -> 𝔹
  validShape' :: a -> 𝔹
  boundedShape :: ℤ² -> a -> 𝔹
  shapeWH :: a -> ℤ²
  alignShape :: a -> a
  area :: a -> ℤ
  showShape :: a -> Text
  showShapes :: [a] -> Text
  showShapess :: [[a]] -> Text
  traceShape :: a -> b -> b

instance ShapeLike (Shape ℤ²) where
  mkShape cs = case toList cs of
    [] -> EmptyShape
    cs ->
      let s = mkSet cs
          lb = biminimum cs
          ub = bimaximum cs
       in if s |≢| cs
            then Invalid
            else Shape (mk cs) s (mkDiffList cs) (lb, ub)

  validShape _ Invalid = False
  validShape _ EmptyShape = True
  validShape wh shape = boundedShape wh shape

  validShape' Invalid = False
  validShape' _ = True

  boundedShape _ Invalid = False
  boundedShape _ EmptyShape = True
  boundedShape (w, h) shape = let (sw, sh) = shapeWH shape in sw ≤ w ∧ sh ≤ h

  shapeWH Invalid = (0, 0)
  shapeWH EmptyShape = (0, 0)
  shapeWH (Shape _ _ _ ((lx, ly), (ux, uy))) = (ux - lx + 1, uy - ly + 1)

  alignShape Invalid = Invalid
  alignShape EmptyShape = EmptyShape
  alignShape shape@(Shape _ _ _ ((lx, ly), (ux, uy))) =
    offsetShape (negate lx) (negate ly) shape

  area shape = (*) $@ shapeWH shape

  showShape Invalid = "invalid"
  showShape EmptyShape = "empty"
  showShape shape@(Shape cs _ _ bs) =
    unlines
      [ tshow (size cs, bs),
        pretty (toG (alignShape shape))
      ]

  showShapes = unlines ∘ fmap showShape

  showShapess = unlines ∘ fmap showShapes

  traceShape s a = traceTextLn (showShape s) a

data LossShape = LossShape {unLossShape :: Shape ℤ²} deriving (Eq, Show)

instance ShapeLike LossShape where
  mkShape cs = LossShape (mkShape @(Shape ℤ²) cs)
  validShape wh (LossShape s) = validShape wh s
  validShape' (LossShape s) = validShape' s
  boundedShape wh (LossShape s) = boundedShape wh s
  shapeWH (LossShape s) = shapeWH s
  alignShape (LossShape s) = LossShape (alignShape s)
  area (LossShape s) = area s
  showShape (LossShape s) = showShape s
  showShapes = unlines ∘ fmap showShape
  showShapess = unlines ∘ fmap showShapes
  traceShape s a = traceTextLn (showShape s) a

instance Ord LossShape where
  compare (LossShape s1) (LossShape s2) =
    let loss shape = shapeWH shape
     in compare (size s1) (size s2)

instance Sizable LossShape where
  size (LossShape s) = size s

instance (Rotatable (Shape ℤ²)) => Rotatable LossShape where
  (↺) (LossShape s) = LossShape $ (↺) s
  (↻) (LossShape s) = LossShape $ (↻) s

instance (HMirrorable (Shape ℤ²)) => HMirrorable LossShape where
  (◐) (LossShape s) = LossShape $ (◐) s

instance (VMirrorable (Shape ℤ²)) => VMirrorable LossShape where
  (◓) (LossShape s) = LossShape $ (◐) s

part1 :: ℤ
part1 = (((possible @(LossQ LossShape) @LossShape (shapess @(LossQ LossShape) @LossShape) <$> take 2 rs) <>?) |.|)
