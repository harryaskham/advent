{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module TwentyEighteen.Solutions where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Data.Char
import Data.Function ((&))
import Data.List
import Data.List.Split
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Ord
import qualified Data.Sequence as SQ
import qualified Data.Set as S
import Data.Time
import qualified Data.Vector as V
import Text.ParserCombinators.ReadP
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()
import Util

tuplify2 :: [a] -> (a, a)
tuplify2 [x, y] = (x, y)

toCoord :: String -> (Int, Int)
toCoord = tuplify2 . fmap read . splitOn ", "

-- Get the top-left, lower-right bounds of the grid.
-- It's safe to ignore all others because anything extending
-- outside the grid is also infinite.
bounds :: [(Int, Int)] -> ((Int, Int), (Int, Int))
bounds cs =
  ( ( fst $ minimumBy (comparing fst) cs,
      snd $ minimumBy (comparing snd) cs
    ),
    ( fst $ maximumBy (comparing fst) cs,
      snd $ maximumBy (comparing snd) cs
    )
  )

-- Manhattan distance between two points
distance :: (Int, Int) -> (Int, Int) -> Int
distance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

-- Get a map from all grid points to the list of all real points and their distances.
-- Takes a bounds modifier so we can look for infinities.
pointDistances :: [(Int, Int)] -> Int -> M.Map (Int, Int) [((Int, Int), Int)]
pointDistances cs boundsMod = M.fromList distances
  where
    ((minX, minY), (maxX, maxY)) = bounds cs
    allPoints = [(x, y) | x <- [minX - boundsMod .. maxX + boundsMod], y <- [minY - boundsMod .. maxY + boundsMod]]
    distances = [(p, [(c, distance c p) | c <- cs]) | p <- allPoints]

-- Gets the closest point from the given distances. If there is a draw then Nothing.
closestPoint :: [((Int, Int), Int)] -> Maybe (Int, Int)
closestPoint ds = if numMins > 1 then Nothing else Just c
  where
    (c, minDistance) = minimumBy (comparing snd) ds
    numMins = length $ filter (\(_, d) -> d == minDistance) ds

-- Transforms the point-map into a map from point to closest.
pointToClosest :: M.Map (Int, Int) [((Int, Int), Int)] -> M.Map (Int, Int) (Maybe (Int, Int))
pointToClosest = fmap closestPoint

-- Gets the largest finite area from the map.
allAreas :: ((Int, Int), (Int, Int)) -> M.Map (Int, Int) (Maybe (Int, Int)) -> [((Int, Int), Int)]
allAreas bounds = sortOn (Down . snd) . M.toList . countMap . removeEdges bounds . mapMaybe snd . M.toList

-- Get rid of any points that lie on the given bounds.
removeEdges :: ((Int, Int), (Int, Int)) -> [(Int, Int)] -> [(Int, Int)]
removeEdges ((minX, minY), (maxX, maxY)) = filter (\(x, y) -> x /= minX && x /= maxX && y /= minY && y /= maxY)

-- You can still be infinite if you are inside the bounds
-- Horrible hack: try with tight bounds and try with loose bounds, then only take those that don't change.

day6_1 :: IO Int
day6_1 = do
  coords <- fmap toCoord . lines <$> readFile "input/2018/6.txt"
  let bs = bounds coords
      pds = pointDistances coords 0
      pds' = pointDistances coords 50
      ptc = pointToClosest pds
      ptc' = pointToClosest pds'
      areas = M.fromList $ allAreas bs ptc
      areas' = M.fromList $ allAreas bs ptc'
   in return $
        snd . head $
          filter (\((_, x, y), _) -> x == y) $
            sortOn (Down . snd) $
              M.toList $
                M.mapKeys (\k -> (k, M.lookup k areas, M.lookup k areas')) areas

day6_2 :: IO Int
day6_2 = do
  coords <- fmap toCoord . lines <$> readFile "input/2018/6.txt"
  let pds = pointDistances coords 0
      pointsToSums = filter (\(p, s) -> s < 10000) $ M.toList $ fmap (sum . fmap snd) pds
   in return $ length pointsToSums

-- Parse out the char->char relationship in the graph from a line.
parseConstraint :: ReadP (Char, Char)
parseConstraint = do
  string "Step "
  from <- count 1 $ satisfy isAlpha
  string " must be finished before step "
  to <- count 1 $ satisfy isAlpha
  string " can begin."
  return (head from, head to)

-- Get list of all nodes
-- Create list of all ready-to-go nodes (never second in a tuple)
-- Select alphabetical first ready node, add to output
-- Find all edges starting from this node and remove
-- Get the ready-to-go nodes again
-- Any nodes remaining once all edges are removed just get consumed in alpha order.

type Node = Char

-- Get all the nodes that could be consumed.
readyNodes :: String -> [(Node, Node)] -> String
readyNodes nodes [] = nodes
readyNodes nodes edges = filter (not . (`elem` dependents)) nodes
  where
    dependents = snd <$> edges

-- Find the optimal ordering without durations.
getOrdering :: [Node] -> [(Node, Node)] -> String -> String
getOrdering nodes [] output = output ++ sort nodes
getOrdering nodes edges output = getOrdering nodes' edges' (output ++ [nextNode])
  where
    nextNode = minimum $ readyNodes nodes edges
    nodes' = nodes \\ [nextNode]
    edges' = filter (\(n, _) -> n /= nextNode) edges

day7_1 :: IO String
day7_1 = do
  ls <- lines <$> readFile "input/2018/7.txt"
  let edges = fst . head . readP_to_S parseConstraint <$> ls
      nodes = nub $ (fst <$> edges) ++ (snd <$> edges)
   in return $ getOrdering nodes edges ""

-- Task assigned and duration left
type Task = (Node, Int)

-- The state of the current system.
data WorkerState = WorkerState
  { _workers :: V.Vector (Maybe Task),
    _t :: Int,
    _nodes :: [Node],
    _edges :: [(Node, Node)]
  }

-- Run one step of the state.
advanceState :: WorkerState -> WorkerState
advanceState = advanceTime . assignAnyReady . clearCompleted

-- Get any completed nodes from the given state.
completedNodes :: WorkerState -> [Node]
completedNodes st = fst <$> filter (\(n, d) -> d == 0) (catMaybes . V.toList $ _workers st)

-- Increment global time by 1, and drop all in-progress by 1
advanceTime :: WorkerState -> WorkerState
advanceTime st = WorkerState ((fmap . fmap . fmap) (subtract 1) (_workers st)) (_t st + 1) (_nodes st) (_edges st)

-- Resets the given worker if it's one of the completed nodes.
resetWorker :: [Node] -> Maybe Task -> Maybe Task
resetWorker _ Nothing = Nothing
resetWorker cNodes (Just (n, d)) = if n `elem` cNodes then Nothing else Just (n, d)

-- Any completed tasks get their worker freed and we get rid of any edges.
clearCompleted :: WorkerState -> WorkerState
clearCompleted st = WorkerState workers (_t st) (_nodes st) edges
  where
    cNodes = completedNodes st
    edges = filter (\(n, _) -> n `notElem` cNodes) (_edges st)
    workers = fmap (resetWorker cNodes) (_workers st)

-- The duration of a task.
nodeDuration :: Node -> Int
nodeDuration n = ord n - 4

-- Assigns a single node if possible.
-- Removes the node from the list.
assignNode :: WorkerState -> Node -> WorkerState
assignNode st n = case V.findIndex isNothing (_workers st) of
  Nothing -> st
  Just i ->
    let newWorkers = (_workers st V.// [(i, Just (n, nodeDuration n))])
        newNodes = filter (/= n) (_nodes st)
     in WorkerState newWorkers (_t st) newNodes (_edges st)

-- Of the ready-to-go nodes, assigns any that are ready.
-- A no-op if we are currently unable to assign.
assignAnyReady :: WorkerState -> WorkerState
assignAnyReady st = foldl assignNode st $ sort $ readyNodes (_nodes st) (_edges st)

-- Is the current state complete?
-- Need all nodes assigned and all workers idle.
isComplete :: WorkerState -> Bool
isComplete st = V.all isNothing (_workers st) && null (_nodes st)

-- Advance the state until all nodes are completed.
advanceUntil :: WorkerState -> WorkerState
advanceUntil st = if isComplete st then st else advanceUntil (advanceState st)

day7_2 :: IO Int
day7_2 = do
  ls <- lines <$> readFile "input/2018/7.txt"
  let edges = fst . head . readP_to_S parseConstraint <$> ls
      nodes = nub $ (fst <$> edges) ++ (snd <$> edges)
      startState = WorkerState (V.replicate 5 Nothing) 0 nodes edges
   in return $ _t (advanceUntil startState) - 1

-- A tree where each node has multiple children and some integer metadata
type Metadata = Int

data Tree = Tree [Tree] [Metadata] deriving (Show)

-- Parse a single tree and return the remainder
parseTree :: [Int] -> (Tree, [Int])
parseTree (0 : numMeta : xs) = (Tree [] (take numMeta xs), drop numMeta xs)
parseTree (numChild : numMeta : xs) = (Tree (reverse children) (take numMeta rem), drop numMeta rem)
  where
    nTrees 0 acc rem = (acc, rem)
    nTrees numChild acc xs = nTrees (numChild - 1) (tree : acc) rem
      where
        (tree, rem) = parseTree xs
    (children, rem) = nTrees numChild [] xs

sumMeta :: Tree -> Int
sumMeta (Tree [] meta) = sum meta
sumMeta (Tree children meta) = sum meta + sum (sumMeta <$> children)

metaValue :: Tree -> Int
metaValue (Tree [] meta) = sum meta
metaValue (Tree children meta) = sum (metaValue <$> [children !! (m - 1) | m <- meta, m > 0, m <= length children])

day8 :: IO ()
day8 = do
  input <- fmap read . words . head . lines <$> readFile "input/2018/8.txt"
  let tree = fst . parseTree $ input
   in do
        print $ sumMeta tree
        print $ metaValue tree

data Game = Game
  { marbles :: SQ.Seq Int,
    scores :: V.Vector Int,
    currentMarble :: Int,
    nextCount :: Int,
    nextPlayer :: Int,
    numPlayers :: Int
  }
  deriving (Show)

-- Creates a new game with the initial marble placed.
newGame :: Int -> Game
newGame numPlayers =
  Game
    { marbles = SQ.fromList [0],
      scores = V.replicate numPlayers 0,
      currentMarble = 0,
      nextCount = 1,
      nextPlayer = 0,
      numPlayers = numPlayers
    }

-- Run a single turn and return what the last marble was worth.
runTurn :: Game -> (Game, Int)
runTurn game =
  if (nextCount game `mod` 23) == 0
    then run23Turn game
    else runRegularTurn game

run23Turn :: Game -> (Game, Int)
run23Turn Game {..} =
  ( Game
      { marbles = marbles',
        scores = scores V.// [(nextPlayer, (scores V.! nextPlayer) + score)],
        currentMarble = (removeIndex + 2) `mod` length marbles',
        nextCount = nextCount + 1,
        nextPlayer = (nextPlayer + 1) `mod` numPlayers,
        numPlayers = numPlayers
      },
    score
  )
  where
    removeIndex = (currentMarble - 9 + length marbles) `mod` length marbles
    score = nextCount + marbles `SQ.index` removeIndex
    marbles' = SQ.deleteAt removeIndex marbles

runRegularTurn :: Game -> (Game, Int)
runRegularTurn Game {..} =
  ( Game
      { marbles = marbles',
        scores = scores,
        currentMarble = (currentMarble + 2) `mod` length marbles',
        nextCount = nextCount + 1,
        nextPlayer = (nextPlayer + 1) `mod` numPlayers,
        numPlayers = numPlayers
      },
    0
  )
  where
    marbles' = SQ.insertAt currentMarble nextCount marbles

-- Runs the game until the last marble score matches the target.
runTurnUntilPoints :: Int -> Game -> IO Game
runTurnUntilPoints pointsTarget game = do
  when (nextCount game `mod` 1000 == 0) (print $ nextCount game)
  if nextCount game == (pointsTarget + 1)
    then return game
    else runTurnUntilPoints pointsTarget nextGame
  where
    (nextGame, points) = runTurn game

day9 :: IO Int
day9 = do
  game <- runTurnUntilPoints 72058 (newGame 426)
  game <- runTurnUntilPoints 7205800 (newGame 426)
  return $ maximum . scores $ game
