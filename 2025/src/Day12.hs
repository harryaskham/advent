module Day12 where

type C' m f s i =
  ( Integral i,
    Unable m,
    Unable f,
    Ord (f (s (i, i))),
    Uniqueable f (s (i, i)),
    Mkable m (f (s (i, i))),
    Mkable m (i, f (s (i, i))),
    Mkable m ([i], f (s (i, i))),
    Coord' i i (i, i),
    Eq (f (s (i, i))),
    Ord (s (i, i)),
    Originable s (i, i),
    Insertable f (s (i, i)),
    Insertable [] (s (i, i)),
    Functor m,
    Applicative m,
    Monad m,
    Alternative m,
    Foldable m,
    Traversable m,
    Ixable Integer m,
    Mkable f (s (i, i)),
    Foldable f,
    Mkable f ℤ,
    Filterable f (s (i, i)),
    Sizable (m ([i], f (s (i, i)))),
    Semigroup (m ℤ²),
    Semigroup (m ℤ),
    Semigroup (f (s (i, i))),
    Monoid (f (s (i, i))),
    Sizable (f ℤ²),
    Arbitrary f ℤ²,
    Arbitrary m (s (i, i)),
    Takeable ℤ m (s (i, i)),
    Sizable (f (s (i, i))),
    Mkable f (ℤ, (i, i)),
    Mkable m (s (i, i)),
    Mkable m ℤ,
    Foldable f,
    ShapeLikeC s i,
    Show i,
    Show (m ℤ),
    Mkable m (".#" ▦ ℤ²),
    Mkable m [ℤ],
    Monoid (m (s (i, i))),
    Eq (s (i, i)),
    Semigroup (m (i, i)),
    Semigroup (m i),
    Mkable m i,
    Arbitrary f (s (i, i)),
    Rotatable (s (i, i)),
    HMirrorable (s (i, i)),
    VMirrorable (s (i, i))
  ) ::
    Constraint

type C m f s i =
  ( C' m f s i,
    ShapeLike s i,
    ShapeLikes m f s i
  ) ::
    Constraint

data Shape a where
  EmptyShape :: Shape a
  Invalid :: Shape a
  Shape :: (Ord a, MkDiffList a) => Seq a -> Set a -> DiffList a -> (a, a) -> Shape a

deriving instance (Show a) => Show (Shape a)

instance (Eq (i, i), Show i, Integral i, Coord' i i (i, i), ShapeLikes [] [] Shape i) => Eq (Shape (i, i)) where
  (==) EmptyShape = \case
    EmptyShape -> True
    _ -> False
  (==) a@(Shape _ _ _ _) =
    let vas = vars @[] @[] @Shape @i a
        vaCss = uniq $ getCs <$> vas
     in \case
          b@(Shape _ _ _ _) ->
            let bO@(Shape csBO _ _ _) = toOrigin b
             in any (≡ csBO) vaCss
          _ -> False
  (==) _ = const False

instance (Ord a, Eq (Shape a)) => Ord (Shape a) where
  compare a@(Shape cs _ _ _) b@(Shape cs' _ _ _)
    | a ≡ b = EQ
    | otherwise = compare cs cs'
  compare EmptyShape EmptyShape = EQ
  compare EmptyShape (Shape _ _ _ _) = LT
  compare (Shape _ _ _ _) EmptyShape = GT
  compare _ _ = LT

instance Sizable (Shape a) where
  size Invalid = 0
  size EmptyShape = 0
  size (Shape cs _ _ _) = size cs

instance (ShapeLike Shape i, Num i, Ord i) => Rotatable (Shape (i, i)) where
  (↺) EmptyShape = EmptyShape
  (↺) Invalid = Invalid
  (↺) (Shape cs s ds bs) = mkShape ((mk @BoundedSet (un cs)) ↺)

  (↻) EmptyShape = EmptyShape
  (↻) Invalid = Invalid
  (↻) (Shape cs s ds bs) = mkShape ((mk @BoundedSet (un cs)) ↻)

instance (ShapeLike Shape i, Num i, Ord i) => HMirrorable (Shape (i, i)) where
  (◐) EmptyShape = EmptyShape
  (◐) Invalid = Invalid
  (◐) (Shape cs s ds bs) = mkShape ((mk @BoundedSet (un cs)) ◐)

instance (ShapeLike Shape i, Num i, Ord i) => VMirrorable (Shape (i, i)) where
  (◓) EmptyShape = EmptyShape
  (◓) Invalid = Invalid
  (◓) (Shape cs s ds bs) = mkShape ((mk @BoundedSet (un cs)) ◓)

instance (Num i, Show i, Integral i, Coord' i i (i, i)) => Originable Shape (i, i) where
  origin = (0, 0)
  toOrigin Invalid = Invalid
  toOrigin EmptyShape = EmptyShape
  toOrigin shape@(Shape _ _ _ ((lx, ly), (ux, uy))) =
    offsetShape (negate lx, negate ly) shape

instance (Originable Shape a) => Originable LossShape a where
  origin = origin @Shape @a
  toOrigin (LossShape s) = LossShape (toOrigin s)

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

part2 :: ℤ = 0

traceV v a = traceGrid (toG v) a

class (Shapes m f s i) => Possible m f s i where
  decomp :: [ℤ] -> ([ℤ], [[ℤ]])
  default decomp :: [ℤ] -> ([ℤ], [[ℤ]])
  decomp ns = unzip $ go [(1, ns)]
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

  decomp1 :: [ℤ] -> ([ℤ], [[ℤ]])
  default decomp1 :: [ℤ] -> ([ℤ], [[ℤ]])
  decomp1 ns = unzip $ swap <$> unMap cs
    where
      nss :: [[ℤ]] = go1 ns
      cs :: Map [ℤ] ℤ = counts @[Integer] @Integer nss
      go1 :: [ℤ] -> [[ℤ]]
      go1 ns
        | all (≡ 0) ns = []
        | otherwise =
            let ns' = [n > 0 ??? 1 $ 0 | n <- ns]
                ns'' = [n - n' | (n, n') <- zip ns ns']
             in ns' : go1 ns''

  possibleDecomposed :: m (f (s (i, i))) -> ((i, i), [ℤ]) -> Maybe (s (i, i))
  default possibleDecomposed :: m (f (s (i, i))) -> ((i, i), [ℤ]) -> Maybe (s (i, i))
  possibleDecomposed shapess r@(wh, ns)
    | size ns' ≡ 1 ∨ all (≤ 1) ns' =
        let res = possible @m @f @s @i shapess' (wh, ns')
         in traceShow (ns, "decomped to", ns', "of", decompNss, size <$> shapess') $
              foldl'
                ( \a ss -> case arb ss of
                    Just s -> traceShape s a
                    Nothing -> traceShow "intermediate failed" a
                )
                res
                shapess'
    | otherwise =
        traceShow (ns, "decomped to", ns', "of", decompNss, size <$> shapess') $
          possibleDecomposed shapess' (wh, ns')
    where
      (ns', decompNss) = decomp1 @m @f @s @i ns
      shapess' = run $ sequence [shapes wh shapess .$. decompNs | decompNs <- mk decompNss]

  possible :: m (f (s (i, i))) -> ((i, i), [ℤ]) -> Maybe (s (i, i))
  default possible :: m (f (s (i, i))) -> ((i, i), [ℤ]) -> Maybe (s (i, i))
  possible shapess r@(wh, ns) =
    run do
      shapes' <- shapes @m @f @s @i wh shapess .$. ns
      pure $ case arb (shapes' |-?-> validShape wh) of
        Nothing -> traceShow "no fit" $ Nothing
        Just shape -> traceShow "fit" ∘ traceShape shape $ Just shape

  possibleSeen :: m (f (s (i, i))) -> ((i, i), [ℤ]) -> Maybe (s (i, i))
  default possibleSeen :: m (f (s (i, i))) -> ((i, i), [ℤ]) -> Maybe (s (i, i))
  possibleSeen shapess r@(wh, ns) =
    let shapes' = run $ shapesSeen @m @f @s @i wh shapess ns
     in case arb (shapes' |-?-> validShape wh) of
          Nothing -> traceShow "no fit" $ Nothing
          Just shape -> traceShow "fit" ∘ traceShape shape $ Just shape

instance (Shapes m f s i) => Possible m f s i

class (C m f s i) => Place m f s i where
  rangeEdge :: (i, i) -> (i, i) -> m (i, i)
  rangeBlock :: (i, i) -> (i, i) -> m (i, i)
  place :: (i, i) -> s (i, i) -> f (s (i, i)) -> f (s (i, i))
  places :: (i, i) -> f (s (i, i)) -> f (s (i, i)) -> f (s (i, i))
  place' :: s (i, i) -> f (s (i, i)) -> f (s (i, i))
  places' :: f (s (i, i)) -> f (s (i, i)) -> f (s (i, i))

instance (C m f s i) => Place m f s i where
  rangeEdge (w0, h0) (w1, h1) =
    [(xO, yO) | xO <- ((0 - w1 - 1) |...| 0) <> ((w1 - w0) |...| (w1 + 1)), yO <- (0 - h1 - 1) |...| (h1 + 1)]
      <> [(xO, yO) | xO <- (0 - w1 - 1) |...| w0, yO <- ((0 - h1 - 1) |...| 0) <> ((h0 - h1 - 1) |...| (h1 + 1))]

  rangeBlock (w0, h0) (w1, h1) =
    [ (xO, yO)
    | xO <- (0 - w1 - 1) |...| (w0 + 1),
      yO <- (0 - h1 - 1) |...| (h0 + 1)
    ]

  place' shape0 shape1s
    | not (validShape (99, 99) shape0) = mempty
    | shape0 ≡ (∅) = foldMap (vars @m @f @s) shape1s
    | otherwise =
        let wh0 = shapeWH shape0
         in traceShow "place'" ∘ traceArb $
              uniq
                ( ( Ł
                      ( \shape01s shape1O ->
                          let wh1 = shapeWH shape1O
                           in ( ( Ł
                                    ( \shape01s shape1 ->
                                        let shape01 = toOrigin (shape0 <> shape1)
                                         in if (validShape (99, 99) shape01) then shape01 |-> shape01s else shape01s
                                    )
                                    shape01s
                                    (offsetShape <$> rangeEdge @m @f @s wh0 wh1 <*> pure shape1O)
                                )
                                  !>
                              )
                      )
                      (∅)
                      (foldMap (vars @m @f @s) shape1s)
                  )
                    !>
                )

  place wh shape0 shape1s
    | shape0 ≡ (∅) = shape1s
    | not (validShape wh shape0) = mempty
    | otherwise =
        let wh0 = shapeWH shape0
         in traceShow "place" ∘ traceArb $
              ( ( Ł
                    ( \shape01s shape1O ->
                        let wh1 = shapeWH shape1O
                         in ( ( Ł
                                  ( \shape01s shape1 ->
                                      let shape01 = toOrigin (shape0 <> shape1)
                                       in if boundedShape wh shape01
                                            then shape01 |-> shape01s
                                            else shape01s
                                  )
                                  shape01s
                                  (offsetShape <$> rangeEdge @m @f @s wh0 wh1 <*> pure shape1O)
                              )
                                !>
                            )
                    )
                    (∅)
                    shape1s
                )
                  !>
              )

  places' shape0Us shape1s = uniq ((Ł (\shape01s shape0U -> ((Ł (<-|) shape01s (place' @m @f @s @i shape0U shape1s)) !>)) (∅) shape0Us) !>)
  places wh shape0Us shape1s = uniq ((Ł (\shape01s shape0U -> ((Ł (<-|) shape01s (place @m @f @s @i wh shape0U shape1s)) !>)) (∅) shape0Us) !>)

class (Place m f s i) => Shapes m f s i where
  shapes :: (i, i) -> m (f (s (i, i))) -> ([ℤ] .->. (f (s (i, i))))

  sss :: [s (i, i)]
  sss = shapess @[] @f @s @i

  compShapes :: m ([i], f (s (i, i)))
  compShapes = mk [([(i ≡ j) ??? 1 $ 0 | j <- range 0 (size $ sss @m @f @s @i)], mk₁ s) | (i, s) <- enum $ sss @m @f @s @i]

  shapePairs :: (i, i) -> m (f (s (i, i))) -> m ([i], f (s (i, i)))
  shapePairs wh shs =
    let shsL = un shs
        n = size shsL
     in mk $ nubOn snd [(sort [i, j], places @m @f @s @i wh (shsL !! i) (shsL !! j)) | i <- range 0 (n - 1), j <- range i (n - 1)]

  expandCompShapes :: m ([i], f (s (i, i))) -> m ([i], f (s (i, i)))
  expandCompShapes cshs =
    let cshsL = un cshs
        n = size cshsL
     in mk $
          nubOn
            snd
            [ (zipWith (+) nsI nsJ, traceShow "excomp" ∘ traceArb $ places' @m @f @s @i shsI shsJ)
            | i <- range 0 (n - 1),
              j <- range i (n - 1),
              let (nsI, shsI) = cshsL !! i,
              let (nsJ, shsJ) = cshsL !! j
            ]

  expandN :: i -> m ([i], f (s (i, i))) -> m ([i], f (s (i, i)))
  expandN n cshs =
    let go 0 cshs = cshs
        go n cshs =
          let cshs' = expandCompShapes cshs
           in traceShow ("expanded", size cshs, "→", size cshs') $ go (n - 1) cshs'
     in go n cshs

  cshs0 :: m ([i], f (s (i, i)))
  cshs0 = compShapes @m @f @s @i

  shapesSeen :: (i, i) -> m (f (s (i, i))) -> ([ℤ] .->. (f (s (i, i))))
  shapesSeen wh shape1ss =
    let go ns
          | all (≡ 0) ns = pure $ mk₁ (∅)
          | any (< 0) ns = pure $ (∅)
          | otherwise =
              foldM
                ( \shape01s (i, n) -> do
                    let ns' = ns !. (i, (n - 1))
                    let shape1s = shape1ss !! i
                    shape0s <- go .$. ns'
                    let shape01s' =
                          foldl'
                            (\shape01s' shape0 -> shape01s <> place @m @f @s wh shape0 shape1s)
                            shape01s
                            shape0s
                    let shape01s'' = shape01s' |-?-> (\s -> boundedShape wh s ∧ contiguous s)
                    pure (uniq $ shape01s <> shape01s'')
                )
                (∅)
                (ns ..#)
     in go

sps = shapePairs @[] @[] @Shape @Integer (6, 6) (pure <$> shapess @[] @[] @Shape @ℤ)

xsh n = expandN @[] @Set @Shape @Integer n (compShapes @[] @Set @Shape @Integer)
xsh n = expandN @[] @Set @Shape @Integer n (compShapes @[] @Set @Shape @Integer)

instance (Place m f s i) => Shapes m f s i where
  shapes wh shape1ss =
    let shape1sVs = [foldMap (vars @m @f @s @i) shape1s | shape1s <- shape1ss]
        go :: [ℤ] .->. f (s (i, i))
        go ns
          | all (≡ 0) ns = pure $ mk₁ (∅)
          | any (< 0) ns = pure (∅)
          | otherwise =
              traceArb
                <$> foldM
                  ( \shape01s (i, n) -> do
                      let ns' = ns !. (i, (n - 1))
                      shape0s <- go .$. ns'
                      let shape1Vs = traceShow "shape1sVss !! i" ∘ traceArb $ shape1ss !! i
                      let shape01s' = traceArb $ places @m @f @s wh shape0s shape1Vs
                      pure (uniq $ shape01s <> shape01s')
                  )
                  (∅)
                  (ns ..#)
     in go

traceArb xs =
  case arb xs of
    Nothing -> traceShow "empty" xs
    Just s -> traceShow ("arb of", size xs) ∘ traceShape s $ xs

type ShapeLikeC s i =
  ( Show i,
    Integral i,
    Coord' i i (i, i),
    Semigroup (s (i, i)),
    Monoid (s (i, i))
  ) ::
    Constraint

class ShapeLike s i where
  mkShape :: (Foldable m) => m (i, i) -> s (i, i)
  validShape :: (i, i) -> s (i, i) -> 𝔹
  boundedShape :: (i, i) -> s (i, i) -> 𝔹
  shapeWH :: s (i, i) -> (i, i)
  offsetShape :: (i, i) -> s (i, i) -> s (i, i)
  area :: (a ~ (i, i)) => s (i, i) -> i
  contiguous :: s (i, i) -> 𝔹
  toG :: s (i, i) -> ".#X" ▦ (i, i)
  showShape :: s (i, i) -> Text
  showShapes :: [s (i, i)] -> Text
  showShapess :: [[s (i, i)]] -> Text
  traceShape :: s (i, i) -> b -> b
  traceShapeId :: s (i, i) -> s (i, i)

instance (ShapeLikeC Shape i) => ShapeLike Shape i where
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
  validShape _ EmptyShape = False
  validShape wh shape = boundedShape wh shape

  boundedShape _ Invalid = False
  boundedShape (w, h) shape = let (sw, sh) = shapeWH shape in sw ≤ w ∧ sh ≤ h

  shapeWH Invalid = (0, 0)
  shapeWH EmptyShape = (0, 0)
  shapeWH (Shape _ _ _ ((lx, ly), (ux, uy))) = (ux - lx + 1, uy - ly + 1)

  offsetShape (x, y) Invalid = Invalid
  offsetShape (x, y) EmptyShape = EmptyShape
  offsetShape (x, y) (Shape cs s ds ((lx, ly), (ux, uy))) =
    let cs' = bimap (+ x) (+ y) <$> cs
     in Shape cs' (mk $ un cs') ds ((lx + x, ly + y), (ux + x, uy + y))

  area shape = (*) $@ shapeWH shape

  showShape Invalid = "Invalid"
  showShape EmptyShape = "EmptyShape"
  showShape shape@(Shape cs _ _ bs) =
    unlines
      [ tshow (size cs, bs),
        pretty (toG (toOrigin shape))
      ]

  showShapes = unlines ∘ fmap showShape

  showShapess = unlines ∘ fmap showShapes

  traceShape s a = traceTextLn (showShape s) a
  traceShapeId s = traceTextLn (showShape s) s

  contiguous Invalid = False
  contiguous EmptyShape = True
  contiguous (Shape cs'@(c :<| _) _ _ _) = go cs (mkSeq [c])
    where
      cs = mkSet (un cs')
      go left (c :<| q)
        | c ∉ cs ∨ c ∉ left = go left q
        | otherwise = go (left ∸ c) (q >< mk (neighborsNoDiags c))
      go left _ = left ≡ (∅)

  toG s =
    let (Shape cs' _ _ ((minX, minY), (maxX, maxY))) = toOrigin @Shape @(i, i) s
        cs = mkSet (toList cs')
     in mkGrid [((x - minX, y - minY), (x, y) ∈ cs ??? (#"#" □) $ (#"." □)) | x <- [minY .. maxX], y <- [minY .. maxY]]

data LossShape a = LossShape {unLossShape :: Shape a} deriving (Show)

instance (Eq (Shape a)) => Eq (LossShape a) where
  (LossShape a) == (LossShape b) = a == b

type instance LossF (LossShape a) = LossShape a

type instance LossF (Integer, (Integer, Integer)) = (Integer, (Integer, Integer))

class (ShapeLike s i) => ShapeLikes m f s i where
  shapess :: m (s (i, i))
  vars :: s (i, i) -> f (s (i, i))

instance
  ( C' m f s i,
    ShapeLike s i,
    Rotatable (s (i, i)),
    HMirrorable (s (i, i)),
    VMirrorable (s (i, i))
  ) =>
  ShapeLikes m f s i
  where
  shapess = [mkShape @s (both fromInteger <$> (p |?> (#"#" □))) | p <- mk (snd <$> ps)]
  vars s =
    let fs = (∘) <$> mk [id, (↻) @(s (i, i)), ((↻) @(s (i, i))) ∘ ((↻) @(s (i, i))), ((↺) @(s (i, i)))] <*> mk [id, ((◓) @(s (i, i))), ((◐) @(s (i, i)))]
     in mk $ (toOrigin <$> (fs <*> [s]))

instance (Show i, Integral i, ShapeLike Shape i, Coord' i i (i, i)) => ShapeLike LossShape i where
  mkShape cs = LossShape (mkShape @Shape @i cs)
  validShape wh (LossShape s) = validShape wh s
  boundedShape wh (LossShape s) = boundedShape wh s
  shapeWH (LossShape s) = shapeWH s
  offsetShape o (LossShape s) = LossShape (offsetShape o s)
  area (LossShape s) = area s
  showShape (LossShape s) = showShape s
  showShapes = unlines ∘ fmap showShape
  showShapess = unlines ∘ fmap showShapes
  traceShape s a = traceTextLn (showShape s) a
  traceShapeId s = traceTextLn (showShape s) s
  contiguous (LossShape s) = contiguous s
  toG (LossShape s) = toG s

instance (Ord i, Integral i, Show i, Coord' i i (i, i)) => Ord (LossShape (i, i)) where
  compare =
    let loss shape = (negate (size shape), both (* area shape) (shapeWH shape))
     in comparing loss

instance Sizable (LossShape a) where
  size (LossShape s) = size s

instance (Rotatable (Shape a)) => Rotatable (LossShape a) where
  (↺) (LossShape s) = LossShape $ (↺) s
  (↻) (LossShape s) = LossShape $ (↻) s

instance (HMirrorable (Shape a)) => HMirrorable (LossShape a) where
  (◐) (LossShape s) = LossShape $ (◐) s

instance (VMirrorable (Shape a)) => VMirrorable (LossShape a) where
  (◓) (LossShape s) = LossShape $ (◓) s

instance Magnitude (Shape a) where
  (|.|) (Shape cs _ _ _) = (cs |.|)

type instance MagnitudeF (Shape a) = Integer

type instance MagnitudeF (LossShape a) = Integer

instance Magnitude (LossShape a) where
  (|.|) (LossShape s) = (s |.|)

instance (Semigroup (Shape a)) => Semigroup (LossShape a) where
  (LossShape a) <> (LossShape b) = LossShape (a <> b)

instance (Monoid (Shape a)) => Monoid (LossShape a) where
  mempty = LossShape mempty

shapessL :: [[Shape ℤ²]] = pure <$> shapess @[] @[] @Shape @ℤ

lossshapessL :: [[LossShape ℤ²]] = pure <$> shapess @[] @[] @LossShape @ℤ

shapessQ :: forall q a. (Insertable q (LossShape (Integer, Integer)), Monoid (q (LossShape (Integer, Integer)))) => [q (LossShape ℤ²)]
shapessQ = (\s -> s |-> (∅)) <$> shapess @[] @[] @LossShape @ℤ

shapessSet :: [Set (Shape ℤ²)] = mkSet ∘ pure <$> sss @[] @Set @Shape @Integer

lossshapessSet :: [Set (LossShape ℤ²)] = mk <$> lossshapessL

-- rs' :: [Maybe (LossShape ℤ²)] = possible @[] @LossQ @LossShape @Integer ss <$> rs

part1 :: ℤ
part1 =
  -- let rs' :: [Maybe (LossShape ℤ²)] = possibleDecomposed @[] @LossQ @LossShape @Integer shapessQ <$> (take 1 rs)
  -- let rs' :: [Maybe (LossShape ℤ²)] = possibleDecomposed @[] @LossQ @LossShape @Integer shapessQ <$> (take 1 rs)
  let rs' :: [Maybe (Shape ℤ²)] = possible @[] @[] @Shape @Integer shapessL <$> (take 1 rs)
   in ((rs' <>?) |.|)

(ps, rs) :: [(ℤ, ".#" ▦ ℤ²)] × [(ℤ², [ℤ])] =
  $(aocx 12)
    -- \$(aoc 12)
    -- \$(aocxn 12 1)
    & (⊏|⊐) @(([(ℤ, ".#" ▦ ℤ²) ⯻ ":\n"] ≠ []) × ([(ℤ² ⯻ "x", [ℤ] ⯻ " ") ⯻ ": "] ≠ []))
