module TwentySixteen.Day22 where

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

viablePairs :: [Node] -> [(Node, Node)]
viablePairs ns = [(a, b) | a <- ns, b <- ns, a /= b, used a > 0, used a <= avail b]

part1 :: IO Int
part1 = do
  ns <- readWithParser nodes <$> input 2016 22
  return . length . viablePairs $ ns

viableConnectedPairs :: [Node] -> [(Node, Node)]
viableConnectedPairs ns = [(a, b) | (a, b) <- viablePairs ns, manhattan (pos a) (pos b) == 1]

fewestSteps :: Map Coord2 Node -> Coord2 -> Int
fewestSteps nodeMap gPos = go (SQ.singleton (gPos, nodeMap, 0)) S.empty
  where
    go ((gPos, nodeMap, steps) SQ.:<| queue) seen
      | gPos == (0, 0) = steps
      | (gPos, nodeMap) `S.member` seen = go queue seen
      | otherwise =
        traceShow (steps, gPos, length queue, length nextStates) $
          go
            (queue SQ.>< SQ.fromList nextStates)
            (S.insert (gPos, nodeMap) seen)
      where
        moveData (n1, n2) =
          ( n1 {used = 0, avail = (avail n1 + used n1)},
            n2 {used = (used n2 + used n1), avail = (avail n2 - used n1)}
          )
        nextStates =
          [ ( if pos n1 == gPos then pos n2 else gPos,
              M.insert (pos n1) n1 . M.insert (pos n2) n2 $ nodeMap,
              steps + 1
            )
            | (n1, n2) <- moveData <$> viableConnectedPairs (M.elems nodeMap)
          ]

part2 :: IO Int
part2 = do
  ns <- readWithParser nodes <$> input 2016 22
  let nodeMap = M.fromList [(pos n, n) | n <- ns]
      maxX = maximum (fst . pos <$> ns)
  return $ fewestSteps nodeMap (maxX, 0)
