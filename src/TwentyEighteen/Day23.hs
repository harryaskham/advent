{-# LANGUAGE TupleSections #-}

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

-- Overestimate of the shortest distance a group of bots' overlapping set has from the origin
heuristic :: Set Nanobot -> (Int, Int, Int)
heuristic bots
  | null bots = (0, 0, 0)
  | otherwise = (negate $ S.size bots, lowerBound bots, upperBound bots)

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

part2 :: IO Int
part2 = do
  bots <- readWithParser nanobots <$> input 2018 23
  --bots <- readWithParser nanobots <$> exampleInput 2018 23
  --print $ M.elems $ length <$> connectionMap bots
  let conn = connectionMap bots
      clique =
        head
          . sortOn (Down . length)
          . maxCliques
          $ conn
  print $ [a `S.member` (conn M.! b) | a <- clique, b <- clique, a /= b]
  return $ botsDistance clique

intersectingPoints :: [Nanobot] -> [Coord3]
intersectingPoints bots = traceShow (radius b) $ foldl' (\ps b -> filter (`seenBy` b) ps) (pointsSeen b) rest
  where
    (b : rest) = sortOn radius bots

pointsSeen :: Nanobot -> [Coord3]
pointsSeen (Nanobot (x, y, z) r) =
  [ (x + xO, y + yO, z + zO)
    | xO <- [negate r .. r],
      yO <- [negate (r - abs xO) .. r - abs xO],
      zO <- [negate (r - abs yO - abs xO) .. r - abs yO - abs xO]
  ]

--return . minimum . fmap (manhattan3 (0, 0, 0) . fst) . head . groupOn snd . sortOn (Down . snd) . M.toList $ M.fromListWith (<>) ((,Sum 1) <$> (pointsSeen =<< bots))

--return . minimum $ manhattan3 (0, 0, 0) <$> (corners =<< intersectReduce (botToCube <$> bots))

-- return $ solve bots

-- 54689610 too high
-- 48202240 too low
