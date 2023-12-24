module Day24 where

import Data.Either.Extra (fromEither)
import Math.LinearEquationSolver
import Math.MFSolve

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

txyzStone :: ((Rational, Rational, Rational), (Rational, Rational, Rational)) -> Rational -> (Rational, Rational, Rational)
txyzStone ((x, y, z), (vx, vy, vz)) t = (x + vx * t, y + vy * t, z + vz * t)

-- vx*x + vy*y + vz*z + c = t
-- c =

-- z = dz/dy y + dz/dx x + c

-- x(t) = xi + vx*t

-- dz / dt = d / dt (dz / dy) y + (dz)

-- we have (x,y,z) as function of t

toLinear ((x, y, z), (vx, vy, vz)) =
  ([vx, vy, vz, 1], -x * vx - y * vy - z * vz)

-- [[vx / vz, vy / vz, ], [-x * vx - y * vy - z * vz]]
-- [ [[vx], [x]],
-- [[vy], [y]],
-- [[vz], [z]]
-- ]

-- smashTime :: Intersection -> Maybe Rational
-- smashTime NoIntersection = Nothing
-- smashTime IntersectAt = undefined

-- each is a 4d line, and we are trying to find a 4d line intersecting all
-- every pair of lines forms a plane
-- every pair of planes traces out a volume...
-- get the hypersurface
-- ie need the plane between the lines, t1 = first line, t2 = second line
-- the plane swept out by the line between endpoints
-- 2 lines:
-- z = vz/vy y + vz/vx x + c

-- line between:
-- z'' = my'' * y'' + mx'' * x'' + c''
-- where
--  my'' * y + mx'' * x + c'' = z => my'' = (z - mx'' * x - c'') / y
--  my'' * y' + mx'' * x' + c'' = z' => my'' = (z' - m''*x' - c'') / y'
-- so my''(y-y') + mx''(x-x') = z-z'
--  my'' = ((z-z')-mx''(x-x')) / (y-y')
-- my'' = (z-z')/(y-y') - (x-x')/(y-y') * mx''
-- also
-- 2*c'' = (z+z') - my''(y+y') - mx''(x+x')
-- 2*c'' = (z+z') - mx''*((z-z')(y+y')/(y-y') - (x-x')(y+y')/(y-y'))  - (x+x')
-- mx'' = 2*c'' - (z+z')  /  ((z-z')(y+y')/(y-y') - (x-x')(y+y')/(y-y'))  - (x+x')

-- (z - mx'' * x - c'') / y = (z-z')/(y-y') - (x-x')/(y-y') * ( 2*c'' - (z+z')  /  ((z-z')(y+y')/(y-y') - (x-x')(y+y')/(y-y'))  - (x+x') )

-- so plane between:
-- z'' = (y''*(z-z')/(y-y') - y''*((x-x')/(y-y') + 1) * mx'' * x'' + c''

-- actual system is the line between endpoints
-- hence the first pass gets us all the lines between all endpoints
-- next pass

solve (e, c) (e', c') =
  traceShow ((e, c), (e', c')) $
    case unsafePerformIO $ solveRationalLinearEqsAll Z3 2 [e, e'] [c, c'] of
      [] -> Nothing
      [[x, y, z, t], [x', y', z', t']] ->
        traceShow ("solutions!", (x, y, z, t), (x', y', z', t')) $
          let dt = t' - t
              dt' = if dt == 0 then 1 else dt
              dxdt = ((x' - x) / dt')
              dydt = ((y' - y) / dt')
              dzdt = ((z' - z) / dt')
           in Just ([dxdt, dydt, dzdt, dt], -dxdt * x - dydt * y - dzdt * z)

intersectReduce' :: [([Rational], Rational)] -> [([Rational], Rational)]
intersectReduce' =
  iterateFix
    ( \surfaces ->
        catMaybes
          [ traceShowId $ solve a b
            | (a, b) <- triPairs surfaces
          ]
    )

intersectReduce :: [([Rational], Rational)] -> [([Rational], Rational)]
intersectReduce =
  iterateFix
    ( \surfaces ->
        [ traceShowId . unjust $ solve a b
          | (a, b) <- zip surfaces (drop 1 surfaces)
        ]
    )

plane :: (Fractional a1, Fractional a2) => ((a2, a2, a2), (a2, a2, a2)) -> ((a1, a1, a1), (a1, a1, a1)) -> a3
plane ((x, y, z), (vx, vy, vz)) ((x', y', z'), (vx', vy', vz')) =
  let (mx, my) = (vz / vx, vz / vy)
      (mx', my') = (vz' / vx', vz' / vy')
      c = z - my * y + mx * x
      c' = z' - my' * y' + mx' * x'
   in undefined

stones :: [((Rational, Rational, Rational), (Rational, Rational, Rational))]
stones =
  $(exampleInput 24)
    |- ( let c3 = toTuple3 <$> count 3 (number <* optional (char ',' >> many (char ' ')))
          in many1 ((,) <$> (c3 <* (string " @" >> many (char ' '))) <*> c3 <* eol) <* eof
       )
    & fmap
      ( \((x, y, z), (vx, vy, vz)) ->
          ( (fromIntegral x, fromIntegral y, fromIntegral z),
            (fromIntegral vx, fromIntegral vy, fromIntegral vz)
          )
      )

-- actually looking for 6 parameters
-- duh
-- so we have
-- sx + svx * t + sy + svy * t + sz * svz * t = x + vx *t + y * vy *t + z * vz + t
-- for each stone
-- so for sx,sy,sz,svx,svy,svz:
-- sx + sy + sz + t * (svx + svy + szy) = t x + y + z + t ( vx + vy + vz)
-- or for any stone we say at time t:

-- solve' :: [((Rational, Rational, Rational), (Rational, Rational, Rational))] -> Either (DepError Rational ()) (Dependencies Rational ())
-- solve' :: [((Integer, Integer, Integer), (Integer, Integer, Integer))] -> Either (DepError SimpleVar Integer) Integer
solve' ins ((ax',ay',az'),(avx',avy',avz')) =
  let [x, y, z, vx, vy, vz] = map (makeVariable . SimpleVar) ["x", "y", "z", "vx", "vy", "vz"]
      [ax, ay, az, avx, avy, avz] = makeConatant <$> [ax', ay', az', avx', avy', avz']
      (Right (v, d)) = flip runSolver noDeps $ do
        forM_
          (zip [0 ..] ins)
          ( \(i, ((sx', sy', sz'), (svx', svy', svz'))) ->
              let [sx, sy, sz, svx, svy, svz] = makeConstant <$> [sx', sy', sz', svx', svy', svz']
                  t = makeVariable . SimpleVar $ "t" <> show i
               in (x + t * vx + y + t * vy + z + t * vz) === (sx + t * svx + sy + t * svy + sz + t * svz)
          )
        x + vx === a + ax
        y + vy === a + ay
        z + vz === a + ay
   in (v, d)

-- sum <$> traverse getValue [x, y, z]

intStone :: ((Rational, Rational, Rational), (Rational, Rational, Rational)) -> ((Integer, Integer, Integer), (Integer, Integer, Integer))
intStone ((x, y, z), (vx, vy, vz)) = ((round x, round y, round z), (round vx, round vy, round vz))

dblStone :: ((Rational, Rational, Rational), (Rational, Rational, Rational)) -> ((Double, Double, Double), (Double, Double, Double))
dblStone ((x, y, z), (vx, vy, vz)) = ((fromRational x, fromRational y, fromRational z), (fromRational vx, fromRational vy, fromRational vz))

part1 :: Int
part1 =
  stones
    & intersections (200000000000000 % 1, 400000000000000 % 1)
    & length

part2 =
  stones
    & fmap dblStone
    & solve'

-- & fmap toLinear
-- & intersectReduce
