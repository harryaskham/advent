module Day24 (part1, part2) where

data ℤ'ersection
  = Noℤ'ersection
  | ℤ'ersectAt (ℚ, ℚ) ℚ ℚ
  | Parallel ℚ ℚ

intersect2d :: (ℚ, ℚ) -> (ℚ, ℚ) -> (ℚ, ℚ) -> (ℚ, ℚ) -> ℤ'ersection
intersect2d (x, y) (vx, vy) (x', y') (vx', vy')
  | m == m' && c == c' = Parallel m c
  | m == m' = Noℤ'ersection
  | t >= 0 && t' >= 0 = ℤ'ersectAt (ix, iy) t t'
  | otherwise = Noℤ'ersection
  where
    m = vy / vx
    c = y - (m * x)
    m' = vy' / vx'
    c' = y' - (m' * x')
    ix = (c' - c) / (m - m')
    iy = m * ix + c
    t = (ix - x) / vx
    t' = (ix - x) / vx

inFuture :: (ℚ, ℚ) -> (ℚ, ℚ) -> (ℚ, ℚ) -> Bool
inFuture (x, y) (vx, vy) (ix, iy) = (ix - x) / vx >= 0 && (iy - y) / vy >= 0

validℤ'ersection :: (ℚ, ℚ) -> (ℚ, ℚ) -> (ℚ, ℚ) -> ℤ'ersection -> Bool
validℤ'ersection _ _ _ Noℤ'ersection = False
validℤ'ersection (low, high) (x, y) (vx, vy) (ℤ'ersectAt (ix, iy) t t') =
  ix >= low && iy >= low && ix <= high && iy <= high && inFuture (x, y) (vx, vy) (ix, iy) && t >= 0 && t' >= 0
validℤ'ersection (low, high) (x, y) (vx, vy) (Parallel m c) =
  let f x = m * x + c
      g y = (y - c) / m
   in any
        (\(a, b) -> validℤ'ersection (low, high) (x, y) (vx, vy) (intersect2d (x, y) (vx, vy) (a, b) (0, 0)))
        [(low, f low), (high, f high), (g low, low), (g high, high)]

intersections :: (ℚ, ℚ) -> [((ℚ, ℚ, ℚ), (ℚ, ℚ, ℚ))] -> [ℤ'ersection]
intersections lowHigh stones =
  [ i
    | (((x, y, _), (vx, vy, _)), ((x', y', _), (vx', vy', _))) <- triPairs stones,
      let i = intersect2d (x, y) (vx, vy) (x', y') (vx', vy'),
      validℤ'ersection lowHigh (x, y) (vx, vy) i,
      validℤ'ersection lowHigh (x', y') (vx', vy') i
  ]

stones :: [((ℚ, ℚ, ℚ), (ℚ, ℚ, ℚ))]
stones =
  $(input 24)
    |- ( let c3 = toTuple3 <$> count 3 ((fromℤ'egral <$> number) <* optional (char ',' >> many (char ' ')))
          in many1 ((,) <$> (c3 <* (string " @" >> many (char ' '))) <*> c3 <* eol) <* eof
       )

part1 :: ℤ'
part1 =
  stones
    & intersections (200000000000000 % 1, 400000000000000 % 1)
    & length

part2 :: ℤ
part2 = z3 do
  [x, y, z, vx, vy, vz] <- traverse mkFreshRealVar ["x", "y", "z", "vx", "vy", "vz"]
  let toConst ((x, y, z), (vx, vy, vz)) = traverse (mkRealNum . fromℚ) [x, y, z, vx, vy, vz]
  forM_
    (zip [0 ..] stones)
    ( \(i, stone) -> do
        t <- mkFreshRealVar ("t" <> show i)
        [x', y', z', vx', vy', vz'] <- toConst stone
        sequence
          [ do
              a <- mkAdd =<< ((p :) . pure) <$> mkMul [t, v]
              b <- mkAdd =<< ((p' :) . pure) <$> mkMul [t, v']
              assert =<< mkEq a b
            | (p, v, p', v') <- [(x, vx, x', vx'), (y, vy, y', vy'), (z, vz, z', vz')]
          ]
    )
  round . unjust . snd <$> withModel (\m -> sum . catMaybes <$> mapM (evalReal m) [x, y, z])
