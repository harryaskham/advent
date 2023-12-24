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
solve' ins ((ax', ay', az'), (avx', avy', avz')) ((ax'', ay'', az''), (avx'', avy'', avz'')) ((ax'''', ay'''', az''''), (avx'''', avy'''', avz'''')) =
  let [x, y, z, vx, vy, vz] = map (makeVariable . SimpleVar) ["x", "y", "z", "vx", "vy", "vz"]
      [ax, ay, az, avx, avy, avz] = makeConstant <$> [ax', ay', az', avx', avy', avz']
      [ax''', ay''', az''', avx''', avy''', avz'''] = makeConstant <$> [ax'', ay'', az'', avx'', avy'', avz'']
      [ax''''', ay''''', az''''', avx''''', avy''''', avz'''''] = makeConstant <$> [ax'''', ay'''', az'''', avx'''', avy'''', avz'''']
      t = makeVariable . SimpleVar $ "t"
      -- t0 = makeVariable . SimpleVar $ "t0"
      t0 = makeConstant 1
      t1 = makeVariable . SimpleVar $ "t1"
      t2 = makeVariable . SimpleVar $ "t2"
   in --
      flip runSolver noDeps $ do
        let ts = [makeVariable . SimpleVar $ "t" <> show i | i <- [2 ..]]
        forM_
          (zip [2 ..] ins)
          ( \(i, ((sx', sy', sz'), (svx', svy', svz'))) ->
              let [sx, sy, sz, svx, svy, svz] = makeConstant <$> [sx', sy', sz', svx', svy', svz']
               in -- t = ts !! i
                  do
                    -- time must be positive
                    -- t - abs t === 0

                    -- there  needs to be some t where we intercept
                    x + t * vx === sx + t * svx
                    y + t * vy === sy + t * svy
                    z + t * vz === sz + t * svz

                    -- when (i > 2) do
                    -- let lastT = ts !! (i - 3)
                    -- let ((sx'', sy'', sz''), (svx'', svy'', svz'')) = ins !! (i - 3)
                    -- [sx''', sy''', sz''', svx''', svy''', svz'''] = makeConstant <$> [sx'', sy'', sz'', svx'', svy'', svz'']
                    ---- the distance between the last and this one matches the time it would take us
                    -- vx * (t - lastT) === (sx + t * svx) - (sx''' + lastT * svx''')
                    -- vy * (t - lastT) === (sy + t * svy) - (sy''' + lastT * svy''')
                    -- vz * (t - lastT) === (sz + t * svz) - (sz''' + lastT * svz''')

                    -- -- moving from our first point to here should also intercept
                    -- ax + (t - t0) * vx === sx + t * svx
                    -- ay + (t - t0) * vy === sy + t * svy
                    -- ay + (t - t0) * vz === sz + t * svz

                    -- -- similarly from second point
                    -- ax''' + (t - t1) * vx === sx + t * svx
                    -- ay''' + (t - t1) * vy === sy + t * svy
                    -- ay''' + (t - t1) * vz === sz + t * svz

                    -- -- similarly from third point
                    -- ax''''' + (t - t2) * vx === sx + t * svx
                    -- ay''''' + (t - t2) * vy === sy + t * svy
                    -- ay''''' + (t - t2) * vz === sz + t * svz

                    -- shouldnt make a diff
                    -- (x + t * vx + y + t * vy + z + t * vz) === (sx + t * svx + sy + t * svy + sz + t * svz)
          )
        t - abs t === 0
        return (x + y + z)

-- vx === (ax - x) / t0
-- vy === (ay - y) / t0
-- vz === (az - z) / t0

---- define time as passage of our thrown one
---- t0 === (ax - x) / vx
-- t1 === (ax''' - x) / vx
-- t2 === (ax''''' - x) / vx

---- we smash into the first at t0
-- x + t0 * vx === ax + t0 * avx
-- y + t0 * vy === ay + t0 * avy
-- z + t0 * vz === az + t0 * avz

-- we smash into the second at t1
-- x + t1 * vx === ax''' + t1 * avx'''
-- y + t1 * vy === ay''' + t1 * avy'''
-- z + t1 * vz === az''' + t1 * avz'''

-- we smash into the third at t2
-- x + t2 * vx === ax''''' + t2 * avx'''''
-- y + t2 * vy === ay''''' + t2 * avy'''''
-- z + t2 * vz === az''''' + t2 * avz'''''

-- t0 - abs t0 === 0
-- t1 - abs t1 === 0
-- t2 - abs t2 === 0

-- vx === ax''' - ax / (t1 - t0)
-- vy === ay''' - ay / (t1 - t0)
-- vz === az''' - az / (t1 - t0)
-- vx === ax''''' - ax''' / (t2 - t1)
-- vy === ay''''' - ay''' / (t2 - t1)
-- vz === az''''' - az''' / (t2 - t1)
-- vx === ax''''' - ax / (t2 - t0)
-- vy === ay''''' - ay / (t2 - t0)
-- vz === az''''' - az / (t2 - t0)

-- -- we are able to cover the distance from the 1st and 2nd collisions
-- ax''' - ax === (t1 - t0) * vx
-- ay''' - ay === (t1 - t0) * vy
-- az''' - az === (t1 - t0) * vz

-- ax + t * vx === ax''' + avx'''
-- ay + t * vy === ay''' + avy'''
-- az + t * vz === az''' + avz'''
-- ax''' + t * vx === ax''''' + avx'''''
-- ay''' + t * vy === ay''''' + avy'''''
-- az''' + t * vz === az''''' + avz'''''

-- x + t * vx === ax''' + t * avx'''
-- y + t * vy === ay''' + t * avy'''
-- z + t * vz === az''' + t * avz'''
-- x + t * vx === ax''''' + t * avx'''''
-- y + t * vy === ay''''' + t * avy'''''
-- z + t * vz === az''''' + t * avz'''''

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

-- we want 24, 13, 10 with -3, 1, 2

-- can do recursive until can solve

-- go rest pos velM =
-- [ go (rest \\ next) (intersectXYZ (pos,vel) next
-- \| next <- rest

intersect3d ((ax', ay', az'), (avx', avy', avz')) ((ax'', ay'', az''), (avx'', avy'', avz'')) =
  let [ax, ay, az, avx, avy, avz] = makeConstant <$> [ax', ay', az', avx', avy', avz']
      [ax''', ay''', az''', avx''', avy''', avz'''] = makeConstant <$> [ax'', ay'', az'', avx'', avy'', avz'']
      t = makeVariable . SimpleVar $ "t"
   in flip runSolver noDeps $ do
        ax + t * avx === ax''' + t * avx'''
        ay + t * avy === ay''' + t * avy'''
        az + t * avz === az''' + t * avz'''
        return (t, (avx, avy, avz))

intersect3dFirst (ax', ay', az') ((ax'', ay'', az''), (avx'', avy'', avz'')) =
  let [ax, ay, az] = makeConstant <$> [ax', ay', az']
      [ax''', ay''', az''', avx''', avy''', avz'''] = makeConstant <$> [ax'', ay'', az'', avx'', avy'', avz'']
      t = makeVariable . SimpleVar $ "t"
      [avx, avy, avz] = makeVariable . SimpleVar <$> ["avx", "avy", "avz"]
   in flip runSolver noDeps $ do
        ax + t * avx === ax''' + t * avx'''
        ay + t * avy === ay''' + t * avy'''
        az + t * avz === az''' + t * avz'''
        return (t, (avx, avy, avz))

go :: ((Double, Double, Double), (Double, Double, Double)) -> Double -> (Double, Double, Double) -> Maybe (Double, Double, Double) -> Set ((Double, Double, Double), (Double, Double, Double)) -> [(((Double, Double, Double), (Double, Double, Double)), Maybe (Double, Double, Double))]
go firstOne t pos velM rest
  | rest == mkSet [] = [(firstOne, velM)]
  | otherwise =
      let f =
            case velM of
              Nothing -> intersect3dFirst pos
              Just v -> intersect3d (pos, v)
       in mconcat
            [ go firstOne (t + t'') (fst next) (Just vel') (rest \\ (mkSet [next]))
              | next <- unSet rest,
                let tE = f next,
                isRight tE,
                let Right (t', vel) = tE,
                let t'' = traceShow t' $ 0,
                let vel' = traceShow vel $ (0, 0, 0)
            ]

goAll as = [go a 1 (x + vx, y + vy, z + vz) Nothing (as \\ (mkSet [a])) | a@((x, y, z), (vx, vy, vz)) <- unSet as]

part2 =
  stones
    & fmap dblStone
    & ( \ins ->
          -- [ traceShowId $ solve' (delete a . delete b . delete c $ ins) a b c
          [ traceShowId $ solve' ins a b c
            | -- \| a <- [ins !! 4],
              -- b <- [ins !! 1],
              -- c <- [ins !! 2],
              a <- ins,
              b <- ins,
              a /= b,
              c <- ins,
              b /= c
          ]
      )
    & take 1
    & partitionEithers

-- & fmap traceShowId
-- & fmap (snd >>> nonlinearEqs)

-- & fmap toLinear
-- & intersectReduce
