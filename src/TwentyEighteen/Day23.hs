{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module TwentyEighteen.Day23 where

import Control.Monad
import Control.Monad.Memo
import Coord
import Data.Algorithm.MaximalCliques
import Data.Bits
import Data.Char
import qualified Data.Foldable as F
import Data.Function
import Data.IORef
import Data.List
import Data.List.Extra
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Data.Ord
import qualified Data.PQueue.Prio.Min as PQ
import Data.Sequence (Seq)
import qualified Data.Sequence as SQ
import Data.Set (Set)
import qualified Data.Set as S
import Data.Tuple.Extra
import Data.Vector (Vector)
import qualified Data.Vector as V
import Debug.Trace
import Grid
import qualified Numeric.AD as AD
import Numeric.Backprop
import qualified Numeric.SGD as SGD
import System.IO.Unsafe
import Text.ParserCombinators.Parsec
import Util

data Nanobot = Nanobot Coord3 Int deriving (Eq, Ord, Show)

radius :: Nanobot -> Int
radius (Nanobot _ r) = r

pos :: Nanobot -> Coord3
pos (Nanobot pos _) = pos

nanobots :: GenParser Char () [Nanobot]
nanobots = do
  ns <- many nanobot
  eof
  return ns
  where
    val = read <$> many1 (oneOf "-0123456789")
    nanobot = do
      [x, y, z] <- between (string "pos=<") (string ">, r=") (val `sepBy` char ',')
      r <- val
      eol
      return $ Nanobot (x, y, z) r

part1 :: IO Int
part1 = do
  bots <- readWithParser nanobots <$> input 2018 23
  let maxBot = maximumOn radius bots
      inRange bot = manhattan3 (pos bot) (pos maxBot) <= radius maxBot
  return . length . filter (inRange) $ bots

seenBy :: Coord3 -> Nanobot -> Bool
seenBy pos (Nanobot botPos r) = manhattan3 pos botPos <= r

botsOverlap :: Nanobot -> Nanobot -> Bool
botsOverlap (Nanobot pos1 r1) (Nanobot pos2 r2) = manhattan3 pos1 pos2 <= r1 + r2

overlappingPointClosestToOrigin :: Set Nanobot -> Coord3
overlappingPointClosestToOrigin = undefined

-- Switch to a PQ keeping track of the best possible / worst possible distance?
-- Then we could kind of do an astar here
-- Underestimate for a cluster would be minimum distance to

worstDistance :: Nanobot -> Int
worstDistance (Nanobot pos r) = manhattan3 (0, 0, 0) pos + r

bestDistance :: Nanobot -> Int
bestDistance (Nanobot pos r)
  | d < 0 = 0
  | otherwise = d
  where
    d = manhattan3 (0, 0, 0) pos - r

lowerBound :: Set Nanobot -> Int
lowerBound bots = S.findMax (S.map bestDistance bots)

upperBound :: Set Nanobot -> Int
upperBound bots = S.findMin (S.map worstDistance bots)

-- when two bots intersect, it's either the case that the old one's best is seen by the new one
-- adding a bot can never make the case any better
-- so we add a bot, it's always the worst-of-bests

-- Its a graph clique problem
-- find the maximum clique all of which see each other
-- then just to lowerbound on those

connectionMap :: [Nanobot] -> Map Nanobot (Set Nanobot)
connectionMap bots =
  S.fromList
    <$> M.fromListWith
      (++)
      [(b1, [b2]) | b1 <- bots, b2 <- bots, b1 /= b2, botsOverlap b1 b2]

maxCliques :: (Eq a, Ord a) => Map a (Set a) -> [[a]]
maxCliques conn = getMaximalCliques (\a b -> b `S.member` (conn M.! a)) (M.keys conn)

botsDistance :: [Nanobot] -> Int
botsDistance bots = maximum (bestDistance <$> bots)

overlapAmount :: Nanobot -> Nanobot -> Int
overlapAmount (Nanobot pos1 r1) (Nanobot pos2 r2) = r1 + r2 - manhattan3 pos1 pos2

-- points must be:
-- - inside each radius
-- - distance to p1 <= r1 && distance to p2 <= r2 && ..., minimise distance to 000
-- - lots of equations i e (xr)

mhat :: [Double] -> [Double] -> Double
mhat [x1, y1, z1] [x2, y2, z2] = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

type D3 = (Double, Double, Double)

m3 :: D3 -> D3 -> Double
m3 (x1, y1, z1) (x2, y2, z2) = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

mkLoss :: [([Double], Double)] -> [Double] -> Double
mkLoss posRs pos = sum (botLoss <$> posRs) + mhat [0.0, 0.0, 0.0] pos
  where
    botLoss (botPos, r) =
      let d = mhat pos botPos in if d <= r then 0.0 else d

part2 :: IO Int
part2 = do
  bots <- readWithParser nanobots <$> input 2018 23
  --bots <- readWithParser nanobots <$> exampleInput 2018 23
  --print $ M.elems $ length <$> connectionMap bots
  --let cs = maxCliques (connectionMap bots)
  let cs = getMaximalCliques botsOverlap bots
  --let cs = cliques botsOverlap bots
  let clique = head . sortOn (Down . length) $ cs
  {-
      overlaps =
        sortOn
          thd3
          [ (b1, b2, overlapAmount b1 b2)
            | (i1, b1) <- zip [0 ..] clique,
              (i2, b2) <- zip [0 ..] clique,
              i2 > i1
          ]
  -}
  -- print $ take 100 $ overlaps
  -- print $ length (filter ((0, 0, 0) `seenBy`) clique)
  -- print $ maximum $ bestDistance <$> clique
  --return $ minimalPoint clique
  --return $ solve clique
  let xyzrs = (\(Nanobot (x, y, z) r) -> (auto $ fromIntegral x, auto $ fromIntegral y, auto $ fromIntegral z, auto $ fromIntegral r)) <$> bots
      botLoss x y z (bx, by, bz, r) = let d = abs (bx - x) + abs (by - y) + abs (bz - z) in if d <= r then 0 else d
      loss (sequenceVar -> [x, y, z]) = sum (botLoss x y z <$> (0, 0, 0, 0) : xyzrs)
      deriv = [\(x, (y, z)) -> let [x', y', z'] = gradBP loss [x, y, z] in (x', (y', z'))]
      start :: (Double, (Double, Double))
      start =
        let (sx, sy, sz) =
              foldl'
                ( \(x, y, z) (Nanobot (x', y', z') _) ->
                    (x + fromIntegral x', y + fromIntegral y', z + fromIntegral z')
                )
                (0.0, 0.0, 0.0)
                bots
            l = fromIntegral $ length bots
         in (sx / l, (sy / l, sz / l))
      result :: (Double, (Double, Double))
      result =
        SGD.run
          (SGD.adam SGD.def id)
          (take 1000000 $ cycle deriv)
          --(0.0, (0.0, 0.0))
          (4007281.321, ((-280509.371), 378799.147)) -- precomputed mean position
      (x, (y, z)) = result
  -- print start
  print (x, y, z)
  return $ manhattan3 (0, 0, 0) (round x, round y, round z)

-- return $ botsDistance clique

-- TODO: now that we have the clique, we can actually find the region of 3d space represented by the turned-cube, and keep intersecting this with all others until we get a region of space
-- Easier if we rotate our axes
-- The issue is that the virtual point is not necessarily on any bot's efficient path to the origin
-- or we do have a system of equations / inequalities now as well

pointsSeen :: Nanobot -> [Coord3]
pointsSeen (Nanobot (x, y, z) r) =
  [ (x + xO, y + yO, z + zO)
    | xO <- [negate r .. r],
      yO <- [negate (r - abs xO) .. r - abs xO],
      zO <- [negate (r - abs yO - abs xO) .. r - abs yO - abs xO]
  ]

corners :: Nanobot -> [Coord3]
corners (Nanobot (x, y, z) r) =
  [ (x + r, y, z),
    (x - r, y, z),
    (x, y + r, z),
    (x, y - r, z),
    (x, y, z + r),
    (x, y, z - r)
  ]

edges :: Nanobot -> [Coord3]
edges (Nanobot (x, y, z) r) =
  traceShow "bot" $
    [ (x + xO, y + yO, z + zO)
      | xO <- [- r .. r],
        yO <- [- r + abs xO .. r - abs xO],
        let zO = r - xO - yO
    ]

solve bots = minimum $ manhattan3 (0, 0, 0) <$> (filter (\c -> all (c `seenBy`) bots) (edges =<< bots))

partitionSpace :: Coord3 -> Coord3 -> [(Coord3, Coord3)]
partitionSpace (lx, ly, lz) (ux, uy, uz) =
  [ ((lx, ly, lz), (hx, hy, hz)),
    ((hx, ly, lz), (ux, hy, hz)),
    ((lx, hy, lz), (hx, uy, hz)),
    ((hx, hy, lz), (ux, uy, hz)),
    ((lx, ly, hz), (hx, hy, uz)),
    ((hx, ly, hz), (ux, hy, uz)),
    ((lx, hy, hz), (hx, uy, uz)),
    ((hx, hy, hz), (ux, uy, uz))
  ]
  where
    hx = (lx + ux) `div` 2
    hy = (ly + uy) `div` 2
    hz = (lz + uz) `div` 2

-- try a BFS of space partitioning looking for the minimum
minimalPoint :: [Nanobot] -> Int
minimalPoint bots = minimum $ manhattan3 (0, 0, 0) <$> go (SQ.singleton (lb, ub, 0)) []
  where
    lb = traceShowId $ minimumOn (manhattan3 (0, 0, 0)) (pos <$> bots)
    ub = traceShowId $ maximumOn (manhattan3 (0, 0, 0)) (pos <$> bots)
    inRegion point = all (\(Nanobot pos r) -> manhattan3 pos point <= r) bots
    go SQ.Empty points = points
    go ((lb, ub, depth) SQ.:<| queue) points
      | manhattan3 lb ub <= 3 = go queue points
      | not (null points)
          && (manhattan3 (0, 0, 0) ub > manhattan3 (0, 0, 0) (head points))
          && (manhattan3 (0, 0, 0) lb > manhattan3 (0, 0, 0) (head points)) =
        go queue points
      | inLb && inUb = go nextQueue (lb : ub : points)
      | inLb = go nextQueue (lb : points)
      | inUb = go nextQueue (ub : points)
      | otherwise = traceShow (length points, depth, length queue) $ go nextQueue points
      where
        nextQueue = queue SQ.>< SQ.fromList ((\(a, b) -> (a, b, depth + 1)) <$> partitionSpace lb ub)
        inLb = inRegion lb
        inUb = inRegion ub

-- TODO: Rotate 90 degrees, treat as cubes, do it by intersection

-- | x-x1| + |y-y1| + |z-z1| <= r1 is equation of manhattansphere

-- TODO erm
-- Can we do this?
-- We can bound the area with the closest-to-origin 8 planes making the irregularly octahedral regino
-- all planes actually exist and are O(1) to compute
-- and if wedo this we get real points of intersection
-- should work due to all 90 degree angles
-- All planes are just defined by slope adn intersection with the axis
-- but. small ones would fuck it up
{-
x + y + z <
pos=<10,12,12>, r=2
pos=<12,14,12>, r=2
pos=<16,12,12>, r=4
pos=<14,14,14>, r=6
pos=<50,50,50>, r=200

this clique gives
10 + a = x, 12 + b = y, 12 + c = z, a + b + c <= 2,
12 + d = x, 14 + e = y, 12 + f = z, d + e + f <= 2,
16 + g = x, 12 + h = y, 12 + i = z, g + h + i <= 4,
14 + j = x, 14 + k = y, 14 + l = z, j + k + l <= 6,
14 + j = x, 14 + k = y, 14 + l = z, j + k + l <= 6,
-}

--return . minimum . fmap (manhattan3 (0, 0, 0) . fst) . head . groupOn snd . sortOn (Down . snd) . M.toList $ M.fromListWith (<>) ((,Sum 1) <$> (pointsSeen =<< bots))

--return . minimum $ manhattan3 (0, 0, 0) <$> (corners =<< intersectReduce (botToCube <$> bots))

-- return $ solve bots

-- 54689610 too high
-- 48202240 too low
