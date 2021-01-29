module TwentyEighteen.Day23 where

import Control.Arrow
import Control.Monad
import Control.Monad.Memo
import Coord
import Data.Bits
import Data.Char
import qualified Data.Foldable as F
import Data.Function
import Data.List
import Data.List.Extra
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
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
import Text.ParserCombinators.Parsec
import Util

data Nanobot = Nanobot Coord3 Int

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

bfs :: [Nanobot] -> Seq Coord3 -> Coord3 -> Int -> Set Coord3 -> Coord3
bfs _ SQ.Empty bestPos _ _ = bestPos
bfs bots (p SQ.:<| queue) bestPos bestBots seen
  | p `S.member` seen = bfs bots queue bestPos bestBots seen
  | nBots > bestBots = bfs bots nextQueue p nBots nextSeen
  | otherwise = traceShow (p) $ bfs bots nextQueue bestPos bestBots nextSeen
  where
    nextSeen = S.insert p seen
    nextQueue = queue SQ.>< (SQ.fromList (neighbors3 p))
    nBots = length ((p `seenBy`) <$> bots)

part2 :: IO Int
part2 = do
  bots <- readWithParser nanobots <$> input 2018 23
  let c = bfs bots (SQ.singleton (0, 0, 0)) (0, 0, 0) 0 S.empty
  return $ manhattan3 (0, 0, 0) c
