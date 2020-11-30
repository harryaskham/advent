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

-- Read signed ints from file.
freqsToNums :: IO [Int]
freqsToNums = do
  content <- readFile "input/2018/1.txt"
  return $ parseLine <$> lines content
  where
    parseLine :: String -> Int
    parseLine (sign : number) =
      case sign of
        '+' -> read number
        '-' -> -1 * read number

day1_1 :: IO Int
day1_1 = sum <$> freqsToNums

day1_2 :: IO Int
day1_2 = do
  nums <- cycle <$> freqsToNums
  return $ next S.empty nums 0
  where
    next :: S.Set Int -> [Int] -> Int -> Int
    next seen (n : ns) frequency =
      if frequency `S.member` seen then frequency else next (S.insert frequency seen) ns (frequency + n)

-- Get a count of unique items in a list.
itemCounts :: Ord a => [a] -> M.Map a Int
itemCounts = foldl (\acc x -> M.insertWith (+) x 1 acc) M.empty

-- Does the list contain the same thing exactly n times?
exactlyN :: Ord a => Int -> [a] -> Bool
exactlyN n xs = n `elem` (snd <$> M.toList (itemCounts xs))

day2_1 :: IO Int
day2_1 = do
  content <- readFile "input/2018/2.txt"
  return $ go 2 (lines content) * go 3 (lines content)
  where
    go :: Int -> [String] -> Int
    go n ls = length $ filter (exactlyN n) ls

-- Get the number of differences between the two strings.
editDistance :: Eq a => [a] -> [a] -> Int
editDistance xs ys = length $ filter (== False) $ fmap (uncurry (==)) (zip xs ys)

-- Gets only the items that are the same in both cases.
common :: Eq a => [a] -> [a] -> [a]
common xs ys = catMaybes $ filter (/= Nothing) $ fmap (\(x, y) -> if x == y then Just x else Nothing) (zip xs ys)

day2_2 :: IO String
day2_2 = do
  ls <- lines <$> readFile "input/2018/2.txt"
  return $ head [common x y | x <- ls, y <- ls, editDistance x y == 1]

-- The details of a fabric rectangle
data FabRect = FabRect
  { _id :: String,
    _xC :: Int,
    _yC :: Int,
    _width :: Int,
    _height :: Int
  }
  deriving (Show)

-- Read a rectangle from #id @ x,y: wxh form.
parseFabricRect :: String -> FabRect
parseFabricRect s = FabRect idStr (read xStr) (read yStr) (read widthStr) (read heightStr)
  where
    [_ : idStr, _, coordStr, dimStr] = words s
    [xStr, yStr] = splitOn "," $ delete ':' coordStr
    [widthStr, heightStr] = splitOn "x" dimStr

-- Keep track of which coords have been covered how many times.
-- Pass in the current counts for each location and the current coord to update it with.
trackOverlap :: M.Map (Int, Int) Int -> (Int, Int) -> M.Map (Int, Int) Int
trackOverlap counts coord = M.insertWith (+) coord 1 counts

coordsForRect :: FabRect -> [(Int, Int)]
coordsForRect fr = [(x, y) | x <- [_xC fr .. _xC fr + _width fr - 1], y <- [_yC fr .. _yC fr + _height fr - 1]]

coordCounts :: [FabRect] -> M.Map (Int, Int) Int
coordCounts frs = itemCounts coords
  where
    coords = concat $ coordsForRect <$> frs

day3_1 :: IO Int
day3_1 = do
  frs <- fmap parseFabricRect . lines <$> readFile "input/2018/3.txt"
  return $ snd <$> M.toList (coordCounts frs) & filter (> 1) & length

-- Is the given rectangle non-overlapping?
isRectUnique :: M.Map (Int, Int) Int -> FabRect -> Bool
isRectUnique coordCounts rect = all (== 1) counts
  where
    coords = coordsForRect rect
    counts = catMaybes $ (`M.lookup` coordCounts) <$> coords

day3_2 :: IO String
day3_2 = do
  frs <- fmap parseFabricRect . lines <$> readFile "input/2018/3.txt"
  return $ _id . head $ filter (isRectUnique (coordCounts frs)) frs

-- Month Day Hour Minute
data TimeStamp = TimeStamp Day TimeOfDay deriving (Eq, Ord, Show)

type GuardId = Int

data LogLine
  = GuardChange GuardId
  | Asleep
  | Awake
  deriving (Eq, Ord, Show)

-- Parse out the raw logs into timestamps and message
parseLogSleep :: ReadP (TimeStamp, LogLine)
parseLogSleep = do
  string "[1518-"
  month <- count 2 digit
  string "-"
  day <- count 2 digit
  string " "
  hours <- count 2 digit
  string ":"
  minutes <- count 2 digit
  string "] "
  logLine <- parseGuard +++ parseAsleep +++ parseAwake
  return (TimeStamp (fromGregorian 1518 (read month) (read day)) (TimeOfDay (read hours) (read minutes) 0), logLine)
  where
    digit = satisfy isDigit
    parseGuard = do
      string "Guard #"
      guardId <- manyTill digit (satisfy (== ' '))
      return $ GuardChange (read guardId)
    parseAsleep = do
      string "falls asleep"
      return Asleep
    parseAwake = do
      string "wakes up"
      return Awake

-- Split the logs into chunks - either only guard changes or only awake/asleep.
splitLogs :: [(TimeStamp, LogLine)] -> [[(TimeStamp, LogLine)]]
splitLogs = drop 1 . split (whenElt isGuard)
  where
    isGuard (_, GuardChange _) = True
    isGuard _ = False

-- Groups split logs into pairs
groupSplitLogs :: [[(TimeStamp, LogLine)]] -> [(GuardId, [(TimeStamp, LogLine)])]
groupSplitLogs ls = (\l -> (getGuard $ head l, l !! 1)) <$> chunksOf 2 ls
  where
    getGuard [(_, GuardChange guardId)] = guardId

-- Gets the total minutes asleep in the given awake/asleep logs.
minutesAsleep :: [(TimeStamp, LogLine)] -> Int
minutesAsleep ls = sum (sleepMinutes <$> chunks)
  where
    chunks = chunksOf 2 (fst <$> ls)
    sleepMinutes [TimeStamp _ t1, TimeStamp _ t2] = length $ getMinutes t1 t2

-- Gets the discrete minutes asleep in the given logs
discreteMinutesAsleep :: [(TimeStamp, LogLine)] -> [Int]
discreteMinutesAsleep ls = concat $ sleepMinutes <$> chunks
  where
    chunks = chunksOf 2 (fst <$> ls)
    sleepMinutes [TimeStamp _ t1, TimeStamp _ t2] = getMinutes t1 t2

-- Gets pairs of (guard ID, minutes asleep) - guard ID could be repeated.
guardMinutesAsleep :: [(GuardId, [(TimeStamp, LogLine)])] -> [(GuardId, [Int])]
guardMinutesAsleep = (fmap . fmap) discreteMinutesAsleep

-- Create a map from guard ID to list of minutes spent asleep.
guardTotals :: [(GuardId, [Int])] -> M.Map GuardId [Int]
guardTotals = foldl (\acc (guardId, minutes) -> M.insertWith (++) guardId minutes acc) M.empty

-- Find the guard with the most total sleep.
sleepiestGuard :: M.Map GuardId [Int] -> GuardId
sleepiestGuard guards = fst $ maximumBy (comparing (length . snd)) $ M.toList guards

-- Find a given guard's most common minute
mostCommonGuardMinute :: M.Map GuardId [Int] -> GuardId -> Int
mostCommonGuardMinute guards guardId = maybe 0 (fst . mostCommon) (M.lookup guardId guards)

-- Get the most common thing along with its count.
mostCommon :: (Ord a, Eq a) => [a] -> (a, Int)
mostCommon xs = maximumBy (comparing snd) $ M.toList (itemCounts xs)

-- Converts a timestamp pair to the list of minute-values it contains
getMinutes :: TimeOfDay -> TimeOfDay -> [Int]
getMinutes (TimeOfDay _ m1 _) (TimeOfDay _ m2 _) = [m1 .. m2 -1]

day4_1 :: IO Int
day4_1 = do
  -- Parse out the timestamps and logline, taking first match and dropped rest of string.
  ls <- sort . fmap (fst . head . readP_to_S parseLogSleep) . lines <$> readFile "input/2018/4.txt"
  let guards = guardTotals . guardMinutesAsleep . groupSplitLogs . splitLogs $ ls
      guard = sleepiestGuard guards
      minute = mostCommonGuardMinute guards guard
   in return $ guard * minute

day4_2 :: IO Int
day4_2 = do
  ls <- sort . fmap (fst . head . readP_to_S parseLogSleep) . lines <$> readFile "input/2018/4.txt"
  let guards = guardTotals . guardMinutesAsleep . groupSplitLogs . splitLogs $ ls
      guardsToMostCommonMinute = (\(g, ms) -> (g, mostCommon ms)) <$> filter (not . null . snd) (M.toList guards)
      (guard, (minute, count)) = maximumBy (comparing $ snd . snd) guardsToMostCommonMinute
   in do
        print guards
        print guardsToMostCommonMinute
        return $ guard * minute

-- Do two chars react with one another?
react :: Char -> Char -> Bool
react x y = x /= y && (toLower x == toLower y)

-- Reduces a polymer by exploding pairs of Aa, Bb etc. Only runs 1 step (e.g. needs applying recursively)
reducePolymer :: String -> String
reducePolymer "" = ""
reducePolymer [x] = [x]
reducePolymer (x : y : xs) =
  if
      | react x y -> reducePolymer xs
      | otherwise -> x : reducePolymer (y : xs)

-- Iterate reductioun until a fixed point.
reduceCompletely :: String -> String
reduceCompletely xs = if reducePolymer xs == xs then xs else reduceCompletely (reducePolymer xs)

day5_1 :: IO Int
day5_1 = do
  p <- head . lines <$> readFile "input/2018/5.txt"
  return $ length . reduceCompletely $ p

-- Gets a string without the given character
without :: Char -> String -> String
without c xs = [x | x <- xs, x /= toLower c, x /= toUpper c]

day5_2 :: IO Int
day5_2 = do
  p <- head . lines <$> readFile "input/2018/5.txt"
  return $ minimum $ length . reduceCompletely <$> (without <$> ['a' .. 'z'] <*> pure p)

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
allAreas bounds = sortOn (Down . snd) . M.toList . itemCounts . removeEdges bounds . mapMaybe snd . M.toList

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
