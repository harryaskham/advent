module Day24 where

data Intersection
  = NoIntersection
  | IntersectAt (Rational, Rational) Rational Rational
  | Parallel Rational Rational
  deriving (Show, Eq, Ord)

intersect2d :: (Rational, Rational) -> (Rational, Rational) -> (Rational, Rational) -> (Rational, Rational) -> Intersection
intersect2d (x, y) (vx, vy) (x', y') (vx', vy')
  | m == m' && c == c' = Parallel m c
  | m == m' = NoIntersection
  | t >= 0 && t' >= 0 = IntersectAt (ix, iy) t t'
  | otherwise = NoIntersection
  where
    m = vy / vx
    c = y - (m * x)
    m' = vy' / vx'
    c' = y' - (m' * x')
    ix = (c' - c) / (m - m')
    iy = m * ix + c
    t = (ix - x) / vx
    t' = (ix - x) / vx

inFuture :: (Rational, Rational) -> (Rational, Rational) -> (Rational, Rational) -> Bool
inFuture (x, y) (vx, vy) (ix, iy) =
  (ix - x) / vx >= 0
    && (iy - y) / vy >= 0

validIntersection :: (Rational, Rational) -> (Rational, Rational) -> (Rational, Rational) -> Intersection -> Bool
validIntersection _ _ _ NoIntersection = False
validIntersection (low, high) (x, y) (vx, vy) (IntersectAt (ix, iy) t t') =
  ix >= low && iy >= low && ix <= high && iy <= high && inFuture (x, y) (vx, vy) (ix, iy) && t >= 0 && t' >= 0
validIntersection (low, high) (x, y) (vx, vy) (Parallel m c) =
  let f x = m * x + c
      g y = (y - c) / m
   in any
        (\(a, b) -> validIntersection (low, high) (x, y) (vx, vy) (intersect2d (x, y) (vx, vy) (a, b) (0, 0)))
        [(low, f low), (high, f high), (g low, low), (g high, high)]

intersections :: (Rational, Rational) -> [((Rational, Rational, Rational), (Rational, Rational, Rational))] -> [Intersection]
intersections lowHigh stones =
  [ i
    | ((a@(x, y, z), (vx, vy, vz)), (b@(x', y', z'), (vx', vy', vz'))) <- triPairs stones,
      let i = intersect2d (x, y) (vx, vy) (x', y') (vx', vy'),
      validIntersection lowHigh (x, y) (vx, vy) i,
      validIntersection lowHigh (x', y') (vx', vy') i
  ]

stones :: [((Rational, Rational, Rational), (Rational, Rational, Rational))]
stones =
  $(input 24)
    |- ( let c3 = toTuple3 <$> count 3 ((fromIntegral <$> number) <* optional (char ',' >> many (char ' ')))
          in many1 ((,) <$> (c3 <* (string " @" >> many (char ' '))) <*> c3 <* eol) <* eof
       )

part1 :: Int
part1 =
  stones
    & intersections (200000000000000 % 1, 400000000000000 % 1)
    & length

part2 :: Integer
part2 = z3 do
  [x, y, z, vx, vy, vz] <- traverse mkFreshRealVar ["x", "y", "z", "vx", "vy", "vz"]
  let toConst ((x, y, z), (vx, vy, vz)) = traverse (mkRealNum . fromRational) [x, y, z, vx, vy, vz]
  forM_
    (zip [0 ..] stones)
    ( \(i, stone) -> do
        [x', y', z', vx', vy', vz'] <- toConst stone
        t <- mkFreshRealVar ("t" <> show i)
        assert =<< mkGe t =<< mkRealNum 0
        sequence
          [ do
              a <- mkAdd =<< ((p :) . pure) <$> mkMul [t, v]
              b <- mkAdd =<< ((p' :) . pure) <$> mkMul [t, v']
              assert =<< mkEq a b
            | (p, v, p', v') <- [(x, vx, x', vx'), (y, vy, y', vy'), (z, vz, z', vz')]
          ]
    )
  (_, Just v) <- withModel (\m -> sum . catMaybes <$> mapM (evalReal m) [x, y, z])
  return (round v)
