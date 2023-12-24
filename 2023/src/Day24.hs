module Day24 where

import Data.Ratio ((%))

-- {-
-- t = x'-x/vx-vx'
-- x'-x/vx-vx' = y'-y/vy-vy'
-- -}
-- collide2d :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer) -> Maybe (Rational, Rational)
-- collide2d (x, y) (vx, vy) (x', y') (vx', vy')
--   | (vx - vx') == 0 || (vy - vy') == 0 = Nothing
--   | xt == yt = Just (fromIntegral x + fromIntegral vx * xt, fromIntegral y + fromIntegral vy * yt)
--   | otherwise = Nothing
--   where
--     xt = traceShowId . traceShow (x, y, vx, vy, x', y', vx', vy') $ ((x' - x) % (vx - vx'))
--     yt = traceShowId $ ((y' - y) % (vy - vy'))

{-
y = (vy/vx)x + c
c = y - (vy/vx)x
y+vy = y'+vy'
m*x + c = m'*x + c'
x(m-m') = c'-c
x = c'-c/m-m'
-}

data Intersection = NoIntersection | IntersectAt (Rational, Rational) Rational Rational | Parallel Rational Rational deriving (Show, Eq, Ord)

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
    |- ( let c3 = toTuple3 <$> count 3 (number <* optional (char ',' >> many (char ' ')))
          in many1 ((,) <$> (c3 <* (string " @" >> many (char ' '))) <*> c3 <* eol) <* eof
       )
    & fmap
      ( \((x, y, z), (vx, vy, vz)) ->
          ( (fromIntegral x, fromIntegral y, fromIntegral z),
            (fromIntegral vx, fromIntegral vy, fromIntegral vz)
          )
      )

-- smashTime :: Intersection -> Maybe Rational
-- smashTime NoIntersection = Nothing
-- smashTime IntersectAt = undefined

part1 :: Int
part1 =
  stones
    & intersections (200000000000000 % 1, 400000000000000 % 1)
    & length

part2 :: Int
part2 =
  stones
    & intersections (-1000000000000000000000000 % 1, 1000000000000000000000000 % 1)
    & length
