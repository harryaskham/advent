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

data Intersection = NoIntersection | IntersectAt (Rational, Rational) | Parallel Rational Rational deriving (Show, Eq, Ord)

intersect2d :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer) -> Intersection
intersect2d (x, y) (vx, vy) (x', y') (vx', vy')
  | m == m' && c == c' = traceShow ((m, c), (m', c')) $ Parallel m c
  | m == m' = traceShow ((m, c), (m', c')) $ NoIntersection
  -- \| m - m' == 0 = ParallelNoIntercept m
  | otherwise = traceShow ((m, c), (m', c'), (ix, iy)) $ IntersectAt (ix, iy)
  where
    m = traceShow (x, y, vx, vy, x', y', vx', vy') $ vy % vx
    c = (fromIntegral y) - (m * fromIntegral x)
    m' = traceShow (m, c) $ vy' % vx'
    c' = (fromIntegral y') - (m' * fromIntegral x')
    ix = traceShow (m', c') $ (c' - c) / (m - m')
    iy = m * ix + c

inFuture :: (Integer, Integer) -> (Integer, Integer) -> (Rational, Rational) -> Bool
inFuture (x, y) (vx, vy) (ix, iy) =
  (ix - fromIntegral x) / fromIntegral vx >= 0
    && (iy - fromIntegral y) / fromIntegral vy >= 0

validIntersection :: (Rational, Rational) -> (Integer, Integer) -> (Integer, Integer) -> Intersection -> Bool
validIntersection _ _ _ NoIntersection = False
validIntersection (low, high) (x, y) (vx, vy) (IntersectAt (ix, iy)) =
  ix >= low && iy >= low && ix <= high && iy <= high && inFuture (x, y) (vx, vy) (ix, iy)
validIntersection (low, high) (x, y) (vx, vy) (Parallel m c) =
  let f x = m * x + c
      g y = (y - c) / m
   in (f low >= low && f low <= high && inFuture (x, y) (vx, vy) (low, f low))
        || (f high >= low && f high <= high && inFuture (x, y) (vx, vy) (high, f high))
        || (g low >= low && g low <= high && inFuture (x, y) (vx, vy) (g low, low))
        || (g high >= low && g high <= high && inFuture (x, y) (vx, vy) (g high, high))

intersections :: (Rational, Rational) -> [((Integer, Integer, Integer), (Integer, Integer, Integer))] -> [Intersection]
intersections lowHigh stones =
  [ i
    | (((x, y, _), (vx, vy, _)), ((x', y', _), (vx', vy', _))) <-
        -- [(a, b) | a <- stones, b <- stones, a /= b]
        triPairs stones,
      let i = intersect2d (x, y) (vx, vy) (x', y') (vx', vy'),
      validIntersection lowHigh (x, y) (vx, vy) i,
      validIntersection lowHigh (x', y') (vx', vy') i
  ]

-- 7129 too low
-- 8773 too low
-- not 21307
-- not 14045
-- not 17599
-- not 17598

part1' =
  $(exampleInput 24)
    |- ( let c3 = toTuple3 <$> count 3 (number <* optional (char ',' >> many (char ' ')))
          in many1 ((,) <$> (c3 <* (string " @" >> many (char ' '))) <*> c3 <* eol) <* eof
       )
    & intersections (7 % 1, 17 % 1)
    & length

part1 =
  $(input 24)
    |- ( let c3 = toTuple3 <$> count 3 (number <* optional (char ',' >> many (char ' ')))
          in many1 ((,) <$> (c3 <* (string " @" >> many (char ' '))) <*> c3 <* eol) <* eof
       )
    & intersections (200000000000000 % 1, 400000000000000 % 1)
    & length

part2 :: Text
part2 = "wtf"
