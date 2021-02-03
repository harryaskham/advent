module TwentySixteen.Day22 where

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

data Node = Node
  { pos :: Coord2,
    used :: Int,
    avail :: Int
  }
  deriving (Eq, Ord)

nodes :: GenParser Char () [Node]
nodes = do
  count 2 (many (noneOf "\n") >> eol)
  ns <- many node
  eof
  return ns
  where
    tVal = do
      val <- read <$> many1 digit
      char 'T'
      return $ val
    node = do
      string "/dev/grid/node-x"
      x <- read <$> many1 digit
      string "-y"
      y <- read <$> many1 digit
      spaces
      tVal
      spaces
      used <- tVal
      spaces
      avail <- tVal
      spaces
      many1 digit
      char '%'
      eol
      return $ Node (x, y) used avail

canMove :: Node -> Node -> Bool
canMove a b = used a > 0 && used a <= avail b

viablePairs :: [Node] -> [(Node, Node)]
viablePairs ns = [(a, b) | a <- ns, b <- ns, a /= b, canMove a b]

part1 :: IO Int
part1 = do
  ns <- readWithParser nodes <$> input 2016 22
  return . length . viablePairs $ ns

astar :: Map Coord2 Node -> Coord2 -> Int
astar nodeMap gPos =
  go (PQ.singleton (h gPos (emptyPos nodeMap) 0) (gPos, nodeMap, 0)) S.empty
  where
    h gPos@(x, y) (ex, ey) steps = (manhattan (0, 0) gPos, abs (y - ey), abs (x - ex), steps)
    emptyPos nodeMap = head . M.keys $ M.filter ((== 0) . used) nodeMap
    go queue seen
      | null queue = 0
      | nodeMap `S.member` seen = go rest seen
      | gPos == (0, 0) = steps
      | otherwise =
        tracePause "" $
          traceStrLn (pretty nodeMap) $
            traceShow (steps, gPos, length queue, length nextStates) $
              go
                ( foldl'
                    ( \q st@(gPos, nextNodeMap, _) ->
                        PQ.insert (h gPos (emptyPos nextNodeMap) (steps + 1)) st q
                    )
                    rest
                    nextStates
                )
                (S.insert nodeMap seen)
      where
        ((_, (gPos, nodeMap, steps)), rest) = PQ.deleteFindMin queue
        moveData (a, b) =
          ( a {used = 0, avail = (avail a + used a)},
            b {used = (used b + used a), avail = (avail b - used a)}
          )
        nodeNeighbors nodeMap a = catMaybes (M.lookup <$> neighborsNoDiags (pos a) <*> pure nodeMap)
        nextStates =
          [ ( if pos a == gPos then pos b else gPos,
              nextNodeMap,
              steps + 1
            )
            | a' <- M.elems nodeMap,
              b' <- nodeNeighbors nodeMap a',
              canMove a' b',
              let (a, b) = moveData (a', b')
                  nextNodeMap = M.insert (pos a) a . M.insert (pos b) b $ nodeMap
          ]

instance Show Node where
  show (Node pos used avail)
    | pos == (30, 0) = "G"
    | pos == (0, 0) = "S"
    | used == 0 = "_"
    | fromIntegral used / fromIntegral (used + avail) < 0.5 = " "
    | fromIntegral used / fromIntegral (used + avail) < 0.7 = "."
    | fromIntegral used / fromIntegral (used + avail) < 0.8 = "o"
    | fromIntegral used / fromIntegral (used + avail) < 0.9 = "O"
    | otherwise = "#"

-- Solves with A* search - overshoots by 2, needs a better heuristic
part2_unfinished :: IO Int
part2_unfinished = do
  ns <- readWithParser nodes <$> input 2016 22
  let nodeMap = M.fromList [(pos n, n) | n <- ns]
      maxX = maximum (fst . pos <$> ns)
  return $ astar nodeMap (maxX, 0)

-- Solves by inspection. We can only swap with empties.
part2 :: IO Int
part2 = do
  ns <- readWithParser nodes <$> input 2016 22
  let nodeMap = M.fromList [(pos n, n) | n <- ns]
  return $
    manhattan (emptyPos nodeMap) (4, 15) -- Distance to gap in "fence"
      + manhattan (4, 15) (30, 0) -- Distance to target
      + 5 * 29 -- Empty-looping required to move target to goal.
  where
    emptyPos nodeMap = head . M.keys $ M.filter ((== 0) . used) nodeMap
