module TwentyEighteen.Day7 where

import Data.Char (isAlpha, ord)
import Data.List (nub, sort, (\\))
import Data.Maybe (catMaybes, isNothing)
import qualified Data.Vector as V
import Text.ParserCombinators.ReadP
  ( ReadP,
    count,
    readP_to_S,
    satisfy,
    string,
  )

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

part1 :: IO String
part1 = do
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

part2 :: IO Int
part2 = do
  ls <- lines <$> readFile "input/2018/7.txt"
  let edges = fst . head . readP_to_S parseConstraint <$> ls
      nodes = nub $ (fst <$> edges) ++ (snd <$> edges)
      startState = WorkerState (V.replicate 5 Nothing) 0 nodes edges
   in return $ _t (advanceUntil startState) - 1
