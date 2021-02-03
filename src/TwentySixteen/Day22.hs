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
  deriving (Show, Eq, Ord)

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

viableConnectedPairs :: [Node] -> Map Coord2 [Coord2]
viableConnectedPairs ns =
  M.fromListWith
    (++)
    [ (pos a, [pos b])
      | (a, b) <- viablePairs ns,
        manhattan (pos a) (pos b) == 1
    ]

-- TODO: A* with a heuristic that favours both distance to G,
-- and estimates of the cost of opening up the path to G.
-- Simple manhattan to G is not tight enough, but cost of opening path up is quite suble
fewestSteps :: Map Coord2 Node -> Coord2 -> Int
fewestSteps nodeMap gPos = go (SQ.singleton (gPos, nodeMap, 0, S.empty, viableConnectedPairs (M.elems nodeMap)))
  where
    go SQ.Empty = 0
    go ((gPos, nodeMap, steps, seen, viable) SQ.:<| queue)
      | gPos == (0, 0) = steps
      -- | (gPos, nodeMap) `S.member` seen = go queue seen
      | otherwise =
        traceShow (steps, gPos, length queue, length nextStates, M.size viable) $
        go (queue SQ.>< SQ.fromList nextStates)
      where
        moveData (a, b) =
          ( a {used = 0, avail = (avail a + used a)},
            b {used = (used b + used a), avail = (avail b - used a)}
          )
        nodeNeighbors nodeMap a = catMaybes (M.lookup <$> neighborsNoDiags (pos a) <*> pure nodeMap)
        nextStates =
          [ ( if pos a == gPos then pos b else gPos,
              nextNodeMap,
              steps + 1,
              nextSeen,
              nextViable
            )
            | aPos <- M.keys viable,
              bPos <- viable M.! aPos,
              not ((aPos, bPos) `S.member` seen),
              let a' = nodeMap M.! aPos
                  b' = nodeMap M.! bPos,
              canMove a' b',
              let (a, b) = moveData (a', b')
                  nextNodeMap = M.insert (pos a) a . M.insert (pos b) b $ nodeMap
                  nextSeen = S.insert (pos a, pos b) seen
                  nextViable =
                    --viableConnectedPairs (M.elems nextNodeMap)
                    viable
                      & M.delete aPos
                      & M.delete bPos
                      & M.unionWith (union) (viableConnectedPairs (a : (nodeNeighbors nextNodeMap a)))
                      & M.unionWith (intersect) (viableConnectedPairs (b : (nodeNeighbors nextNodeMap b)))
          ]

part2 :: IO Int
part2 = do
  ns <- readWithParser nodes <$> input 2016 22
  --ns <- readWithParser nodes <$> exampleInput 2016 22
  let nodeMap = M.fromList [(pos n, n) | n <- ns]
      maxX = maximum (fst . pos <$> ns)
  return $ fewestSteps nodeMap (maxX, 0)
