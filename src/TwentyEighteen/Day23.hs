{-# LANGUAGE TupleSections #-}

module TwentyEighteen.Day23 where

import Control.Monad
import Control.Monad.Memo
import Coord
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

-- TODO: Fuck me. it's not cubes, it's radii of manhattan distance
type Cube = (Coord3, Coord3)

intersectAll :: [Cube] -> Maybe Cube
intersectAll [c] = Just c
intersectAll (c1 : c2 : cs) = case c1 `cubeIntersect` c2 of
  Just c -> intersectAll (c : cs)
  Nothing -> Nothing

-- TODO: Wrong! Could be that an edge is closest
distanceToOrigin :: Cube -> Int
distanceToOrigin = minimum . fmap (manhattan3 (0, 0, 0)) . corners

overlappingPointClosestToOrigin :: Set Nanobot -> Coord3
overlappingPointClosestToOrigin = undefined

-- TODO: could do astar always only looking at the biggest groups so far?
dfs :: [Nanobot] -> Int
dfs bots = unsafePerformIO $ do
  best <- newIORef []
  seen <- newIORef (S.empty)
  traverse (\b -> go ((S.singleton b), S.fromList (delete b bots)) seen best) bots
  bestBots <- readIORef best
  let botList = S.toList <$> bestBots
      points = (pointsSeen <$$> botList)
      inters = concat $ foldl1 intersect <$> points
      distances = manhattan3 (0, 0, 0) <$> inters
  return (minimum distances)
  where
    go (botSet, remaining) seenRef bestRef = do
      seen <- readIORef seenRef
      best <- readIORef bestRef
      when (not $ null best) $ print (S.size $ head best)
      let nextBest =
            if null best || S.size (head best) == S.size botSet
              then botSet : best
              else if S.size botSet > S.size (head best) then [botSet] else best
      let nextStates = do
            b <- S.toList remaining
            guard $ all (botsOverlap b) botSet
            let nextBotSet = S.insert b botSet
            return (nextBotSet, S.filter (\b -> all (== True) (botsOverlap b `S.map` nextBotSet)) remaining)
      if botSet `S.member` seen
        then return ()
        else do
          writeIORef bestRef nextBest
          writeIORef seenRef (S.insert botSet seen)
          sequence_ $ go <$> nextStates <*> pure seenRef <*> pure bestRef

{-
bfs :: [Cube] -> Set Cube
bfs (c:ubes) = go (SQ.singleton (S.singleton c, c, S.fromList ubes)) (S.singleton c) (S.singleton c)
  where
    go SQ.Empty _ best = best
    go ((cubeSet, int, remaining) SQ.:<| queue) seen best
      | cubeSet `S.member` seen = go queue seen best
      | otherwise = go nextQueue nextSeen nextBest
      where
        nextSeen = S.insert cubeSet seen
        nextStates = do
          c <- S.toList remaining
          let int' = int `cubeIntersect` c
          guard $ isJust int'
          let Just int'' == int'
          return (S.insert c cubeSet, int'', S.delete c remaining)
        nextQueue = queue SQ.>< SQ.fromList nextStates
        nextBest =
-}

cubeIntersect :: Cube -> Cube -> Maybe Cube
cubeIntersect ((x1, y1, z1), (x1', y1', z1')) ((x2, y2, z2), (x2', y2', z2'))
  | x1 > x2' || y1 > y2' || z1 > z2' || x2 > x1' || y2 > y1' || z2 > z1' = Nothing
  | otherwise = Just ((max x1 x2, max y1 y2, max z1 z2), (min x1' x2', min y1' y2', min z1' z2'))

intersections :: [Cube] -> [Cube]
intersections cubes = do
  (i1, c1) <- zip [0 ..] cubes
  (i2, c2) <- zip [0 ..] cubes
  guard (i1 < i2)
  let int = cubeIntersect c1 c2
  guard (isJust int)
  let Just int' = int
  return int'

intersectReduce :: [Cube] -> [Cube]
intersectReduce cubes = go cubes
  where
    go cubes
      | null nextCubes = cubes
      | otherwise = traceShow (length nextCubes) $ go nextCubes
      where
        nextCubes = intersections cubes

pairwiseIntersections :: [Cube] -> [(Cube, [Cube], Int)]
pairwiseIntersections cubes = do
  (i1, c1) <- zip [0 ..] cubes
  (i2, c2) <- zip [0 ..] cubes
  guard (i1 < i2)
  guard (c1 /= c2)
  let int = cubeIntersect c1 c2
  guard (isJust int)
  let Just int' = int
  return (int', delete c1 . delete c2 $ cubes, 2)

botToCube :: Nanobot -> Cube
botToCube (Nanobot (x, y, z) r) = ((x - r, y - r, z - r), (x + r, y + r, z + r))

-- TODO: Maybe keep track of the indices of intersections instead?
-- No costly list deletion but is still going to blow up

-- TODO: Can we do something greeedy by sorting the cubes?
-- Working outwards, the point we are looking for has to be the bottom-left of some cube
-- Oh, so can we just consider all the bottom-lefts?

corners :: Cube -> [Coord3]
corners ((x, y, z), (x', y', z')) =
  [ (x, y, z),
    (x', y, z),
    (x, y', z),
    (x, y, z'),
    (x', y', z),
    (x', y, z'),
    (x, y', z'),
    (x', y', z')
  ]

pointsOfIntersection :: [Cube] -> [Coord3]
pointsOfIntersection cubes =
  nub $ corners =<< catMaybes [c1 `cubeIntersect` c2 | c1 <- cubes, c2 <- cubes, c1 /= c2]

solve :: [Nanobot] -> Int
solve bots =
  manhattan3 (0, 0, 0)
    . fst
    . head
    . sortOn (\(p, n) -> (negate n, manhattan3 (0, 0, 0) p))
    . M.toList
    $ pointToN
  where
    pointToN =
      traceShowId $
        M.fromListWith
          (<>)
          [(p, Sum 1) | b <- bots, p <- pointsOfIntersection (botToCube <$> bots), p `seenBy` b]

pointsSeen :: Nanobot -> [Coord3]
pointsSeen (Nanobot (x, y, z) r) =
  [ (x + xO, y + yO, z + zO)
    | xO <- [negate r .. r],
      yO <- [negate (r - abs xO) .. r - abs xO],
      zO <- [negate (r - abs yO - abs xO) .. r - abs yO - abs xO]
  ]

-- TODO: Need to do this muuuuch faster

part2 :: IO Int
part2 = do
  bots <- readWithParser nanobots <$> input 2018 23
  --bots <- readWithParser nanobots <$> exampleInput 2018 23
  return $ dfs bots

--return . minimum . fmap (manhattan3 (0, 0, 0) . fst) . head . groupOn snd . sortOn (Down . snd) . M.toList $ M.fromListWith (<>) ((,Sum 1) <$> (pointsSeen =<< bots))

--return . minimum $ manhattan3 (0, 0, 0) <$> (corners =<< intersectReduce (botToCube <$> bots))

-- return $ solve bots

-- 54689610 too high
