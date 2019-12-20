{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiWayIf #-}

module Day18 where

import qualified Data.Set as S
import Data.List
import Data.Char
import Data.List.Split hiding (condense)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Function ((&))
import Control.Applicative
import qualified Data.Vector as V
import qualified Data.Tree as T
import qualified Data.List.Safe as LS
import Control.Monad
import Data.Ord
import Control.Lens hiding (Empty)
import qualified Data.Vector.Split as VS
import Data.Ratio
import Data.Foldable
import Text.ParserCombinators.ReadP
import Debug.Trace
import qualified Data.Matrix as MX
import System.IO
import System.IO.HiddenChar
import Control.Concurrent
import System.Random
import Control.Exception
import TwentyNineteen (unsafeJ, mDistance)
import qualified Data.PQueue.Prio.Min as PQ

data Space = Entrance | Empty | Door Char | KeySpace Char | Wall deriving (Eq)
type Grid = V.Vector (V.Vector Space)

instance Show Space where
  show Entrance = "."
  show Empty = "."
  show (Door c) = [c]
  show (KeySpace c) = [c]
  show Wall = "#"

printGrid :: (Int, Int) -> Grid -> IO ()
printGrid (x, y) grid = sequenceA_ $ print . concat . V.toList <$> charGrid V.// [(y, updatedYth)]
  where
    charGrid = (fmap.fmap) show grid
    ythRow = charGrid V.! y
    updatedYth = ythRow V.// [(x, "@")]

isKey (KeySpace _) = True
isKey _ = False

gridFind :: Space -> Grid -> [(Int, Int)]
gridFind a grid = coords
  where
    nrows = V.length grid
    ncols = V.length (grid V.! 0)
    coords = [(x, y) | x <- [0..ncols-1], y <- [0..nrows-1], grid V.! y V.! x == a]

fromChar :: Char -> Space
fromChar '@' = Entrance
fromChar '.' = Empty
fromChar '#' = Wall
fromChar c
  | isUpper c = Door c
  | otherwise = KeySpace c

newtype Key = Key Char deriving (Ord, Eq, Show)
data StepState = NumSteps Int | Dead deriving (Eq, Show, Ord)
-- Store the position, inventory and number of steps taken.
data Explorer = Explorer Grid (Int, Int) (S.Set Key) StepState (S.Set (Int, Int)) DistanceMap

-- Okay so:
-- Treat as a typical search where if we hit a door without having the key, that's definitely not the shortest path
-- Run a DFS or BFS of states, keeping track of where we have been
-- If we must phit a door we can't open, terminate that branch
-- Don't terminate if we just go past one though
-- If we hit a dead end, also terminate that branch
-- If we pick up a key, add to inventory. If we have 26 keys, terminate.
-- Picking up a key blows open the search space again, so we should then search the entire grid again by resetting seen positions.

bestStepState :: [StepState] -> StepState
bestStepState ss = if all (==Dead) ss
                      then Dead
                      else minimumBy (comparing $ \(NumSteps i) -> i) . filter (/= Dead) $ ss

addStep Dead = Dead
addStep (NumSteps i) = NumSteps (i+1)

vreplace2 :: (Int, Int) -> a -> V.Vector (V.Vector a) -> V.Vector (V.Vector a)
vreplace2 (x, y) a g = g V.// [(y, row)]
  where
    row = (g V.! y) V.// [(x, a)]

-- Return the grid with the given door unlocked.
unlockDoor :: Key -> Grid -> Grid
unlockDoor (Key c) grid = withoutDoor
  where
    keyLoc = gridFind (KeySpace $ toLower c) grid
    withoutKey = case keyLoc of
                   [k] -> vreplace2 k Empty grid
                   [] -> grid
    doorLoc = gridFind (Door $ toUpper c) grid
    withoutDoor = case doorLoc of
                    [k] -> vreplace2 k Empty withoutKey
                    [] -> withoutKey

isDoor :: Space -> Bool
isDoor space = case space of
                 (Door _) -> True
                 _ -> False

numKeys :: Grid -> Int
numKeys grid = length [(x, y) | x <- [0..ncols-1], y <- [0..nrows-1], isKey (grid V.! y V.! x)]
  where
    nrows = V.length grid
    ncols = V.length (grid V.! 0)

overCap :: StepState -> StepState -> Bool
overCap (NumSteps cap) (NumSteps now) = now >= cap
overCap Dead _ = False

-- An A* search heuristic.
-- TODO: Maybe we can precalculate an all-points access map for quick "real" distance lookups
-- Estimates cheapest path length to the finish
-- Best we can do here is nkeys - havekeys, since they might all just be lined up
-- TODO: We can get a better heuristic here by using the manhattan distance to remaining keys.
--       But it's then quite a costly traveling salesman problem to get min-manhattan
--       Or, can we just sum the manhattans? Because it's zero sum, moving towards one moves away from another.
--       No, can't do that, need to underestimate not overestimate.
--       So that is a shortest-euclidian-traversal of all keys.
heuristic :: M.Map Char (Int, Int) -> Explorer -> Int
heuristic keyLocations (Explorer _ pos keys numSteps _ distanceMap) =
  --sum $ dist <$> remainingKeyLocs <*> remainingKeyLocs
  sum $ dist <$> ZipList remainingKeyLocs <*> ZipList (drop 1 remainingKeyLocs)
  --sum $ dist pos <$> remainingKeyLocs
  where
    dist :: (Int, Int) -> (Int, Int) -> Int
    --dist (x1,y1) (x2,y2) = floor $ sqrt . fromIntegral $ ((x2-x1)^2) * ((y2-y1)^2)
    --dist = mDistance
    dist from to = unsafeJ $ M.lookup (from,to) distanceMap
    remainingKeyLocs :: [(Int, Int)]
    remainingKeyLocs = snd <$> M.toList (foldl' (\acc (Key k) -> M.delete k acc) keyLocations (S.toList keys))

-- Get the current distances to all keys cache.
-- This is a cache keyed by point, storing its distance to all keys
-- We need to reset this whenever we find a key (or maybe not... try both TODO)
bestDistance :: (Int, Int) -> (Int, Int) -> Grid -> S.Set (Int, Int) -> Int
bestDistance (x, y) (toX, toY) grid seen
  | (x, y) == (toX, toY) = 0
  | null nextPos = 100000000
  | otherwise = 1 + minimum (bestDistance <$> nextPos <*> pure (toX, toY) <*> pure grid <*> [S.insert (x,y) seen])
  where
    nrows = V.length grid
    ncols = V.length (grid V.! 0)
    validNextPos (x,y) = x >= 0 && y >= 0 && x < ncols && y < nrows
                         && grid V.! y V.! x /= Wall
                         && not ((x,y) `S.member` seen)
                         -- && not (isDoor $ grid V.! y V.! x)
    nextPos = filter validNextPos [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]

bestDistances :: (Int, Int) -> M.Map Char (Int, Int) -> Grid -> M.Map ((Int, Int), (Int, Int)) Int
bestDistances origin keyLocations grid =
  M.fromList [((from,to), bestDistance from to grid S.empty) | from <- froms, to <- tos]
  where
    tos = snd <$> M.toList keyLocations
    froms = origin:tos

getNumSteps (NumSteps i) = i

type DistanceMap = M.Map ((Int, Int), (Int, Int)) Int
type KeyLocations = M.Map Char (Int, Int)

dbg = False

-- Can do something like:
-- find all reachable keys with distances
-- teleport to a key (we cached distance)
-- repeat
-- search over this

keyDistance :: (Int, Int) -> (Int, Int) -> Grid -> S.Set (Int, Int) -> Maybe Int
keyDistance (x, y) (toX, toY) grid seen
  | (x, y) == (toX, toY) = Just 0
  | null nextPos = Nothing
  | all isNothing branches = Nothing
  | otherwise = Just (1 + minimum (catMaybes branches))
  where
    nrows = V.length grid
    ncols = V.length (grid V.! 0)
    validNextPos (x,y) = x >= 0 && y >= 0 && x < ncols && y < nrows
                         && grid V.! y V.! x /= Wall
                         && not ((x,y) `S.member` seen)
                         && not (isDoor $ grid V.! y V.! x)
    nextPos = filter validNextPos [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]
    branches :: [Maybe Int]
    branches = keyDistance <$> nextPos <*> pure (toX, toY) <*> pure grid <*> [S.insert (x,y) seen]

reachableKeys :: S.Set Key -> (Int, Int) -> KeyLocations -> Grid -> M.Map (Int, Int) Int
reachableKeys keys from keyLocations grid =
  unsafeJ <$> M.filter (> Just 0) (M.fromList [(to, keyDistance from to grid S.empty) | to <- remainingKeyLocs])
  where
    remainingKeyLocs :: [(Int, Int)]
    remainingKeyLocs = snd <$> M.toList (foldl' (\acc (Key k) -> M.delete k acc) keyLocations (S.toList keys))

-- Okay, we added capping, didn't help
-- Maybe this needs to be a BFS... means that we find a single solution early
-- So that we can use this to cap a DFS
-- Okay BFS does not help, never reaches 26 so can't cap
-- Proceeds through steps very slowly
-- Need to crack the branching example first

-- One more thing  to try:
-- Cache (position,keyset) -> reachable keys
-- This will speed up the only actual costly thing
-- Key is position and keys, value is the reachable key map
type Cache = M.Map ((Int, Int), S.Set Key) (M.Map (Int, Int) Int)

-- Returns the minimum number of steps to get all the keys.
stepExplorer :: Cache -> M.Map Char (Int, Int) -> StepState -> Int -> PQ.MinPQueue Int Explorer -> IO StepState
--stepExplorer stepCap _ [] = return stepCap
stepExplorer cache keyLocations stepCap nkeys queue
  | PQ.null queue = return stepCap
  | otherwise = do--(Explorer grid (x, y) keys numSteps seenLocations:rest) = do
  -- TODO handle null queue
  when dbg $ do
    printGrid (x, y) grid
    print $ "Location: " ++ show (x, y)
    print $ "Going next: " ++ show nextPos
    print $ "Current keys: " ++ show keys
    print $ "Current steps: " ++ show numSteps
    print $ "Current seen: " ++ show seenLocations
    print $ "Keys: " ++ show (length keys)
    print $ "Best: " ++ show stepCap
  if S.size nextKeys == nkeys
     -- If we found all the keys, update the best cap.
     --then stepExplorer (minimum [stepCap, numSteps]) nkeys rest
     then do
       let newCap = minimum [stepCap, numSteps]
       print newCap
       stepExplorer cache keyLocations newCap nkeys queueWithoutCurrent
     -- Otherwise if we hit a dead end or we already found a nice branch, die
     else if null nextPos || overCap stepCap numSteps
     --then stepExplorer (minimum [stepCap, Dead]) nkeys rest
     then stepExplorer cache keyLocations stepCap nkeys queueWithoutCurrent
     -- Otherwise kick off a few branches.
     else --do
       -- Introduce capping.
       -- For all below need to do as a fold where cap is passed on.
       -- Find bestUp with no cap.
       -- Then bestDown with a truncation of best so far
       -- Then bestRight with trunncation of best so far
       -- Finally bestLeft with same.
       --let branches =
       --      sequenceA
       --      ((\pos -> stepExplorer nkeys (Explorer nextGrid pos nextKeys (addStep numSteps) nextSeenLocations)) <$> nextPos)
       -- in bestStepState <$> branches
         {-
       bestUp <- if validNextPos (x,y-1)
                    then stepExplorer Dead nkeys (Explorer nextGrid (x,y-1) nextKeys (addStep numSteps) nextSeenLocations)
                    else return Dead
       bestDown <- if validNextPos (x,y+1)
                      then stepExplorer bestUp nkeys (Explorer nextGrid (x,y+1) nextKeys (addStep numSteps) nextSeenLocations)
                      else return Dead
       bestLeft <- if validNextPos (x-1,y)
                      then stepExplorer (minimum [bestUp, bestDown]) nkeys (Explorer nextGrid (x-1,y) nextKeys (addStep numSteps) nextSeenLocations)
                      else return Dead
       bestRight <- if validNextPos (x+1,y)
                      then stepExplorer (minimum [bestUp, bestDown, bestLeft]) nkeys (Explorer nextGrid (x+1,y) nextKeys (addStep numSteps) nextSeenLocations)
                      else return Dead
       return $ bestStepState [bestUp, bestDown, bestLeft, bestRight]
       -}
       
       --let nextStates = (\pos -> Explorer nextGrid pos nextKeys (addStep numSteps) nextSeenLocations distanceMap) <$> nextPos
       let rKeys = fromMaybe (reachableKeys keys (x, y) keyLocations nextGrid) $ M.lookup ((x,y),keys) cache
           nextCache = M.insert ((x,y),keys) rKeys cache
           nextStates = (\(pos,d) -> Explorer nextGrid pos nextKeys (NumSteps (d + getNumSteps numSteps)) nextSeenLocations distanceMap) <$> M.toList rKeys
           nextQueue = foldl' (\q ex@(Explorer _ _ _ (NumSteps st) _ _) -> PQ.insert (st + heuristic keyLocations ex) ex q) queueWithoutCurrent nextStates
        --in stepExplorer stepCap nkeys nextRest
        in do
          when dbg $ do
            print $ "Cache length: " ++ show (M.size cache)
            print $ "Reachable keys: " ++ show rKeys
            let (_,nxt) = PQ.findMin nextQueue
            print $ "Heuristic: " ++ show (heuristic keyLocations nxt)
            _ <- if dbg then getLine else return ""
            return ()
          stepExplorer nextCache keyLocations stepCap nkeys nextQueue
  where
    ((_, Explorer grid (x, y) keys numSteps seenLocations distanceMap), queueWithoutCurrent) = PQ.deleteFindMin queue
    -- If current space is key, pick it up, add to inventory, and unlock corresponding door.
    currentSpace = grid V.! y V.! x
    nextKeys = case currentSpace of
                 (KeySpace c) -> S.insert (Key c) keys
                 _ -> keys
    nextGrid = case currentSpace of
                 (KeySpace c) -> unlockDoor (Key c) grid
                 _ -> grid
    nextDistanceMap = distanceMap--case currentSpace of
                        --(KeySpace _) -> bestDistances keyLocations nextGrid
                        --_ -> distanceMap
    -- If we unlocked a door we could potentially have to go anywhere, so reset the seen spaces.
    nextSeenLocations = case currentSpace of
                          (KeySpace _) -> S.singleton (x, y)
                          _ -> S.insert (x, y) seenLocations
    -- Generate all possible locations to search next. Don't go anywhere we already saw, or a wall.
    -- Also don't go through a door.
    -- If there is nowhere to go, we're a dead end.
    nrows = V.length grid
    ncols = V.length (grid V.! 0)
    validNextPos (x,y) = x >= 0 && y >= 0 && x < ncols && y < nrows
                         && not ((x,y) `S.member` nextSeenLocations)
                         && grid V.! y V.! x /= Wall
                         && not (isDoor $ grid V.! y V.! x)
    nextPos = filter validNextPos [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]

day18 :: IO ()
day18 = do
  ls <- lines <$> readFile "input/2019/18.txt"
  --ls <- lines <$> readFile "input/2019/18_example.txt"
  let grid = V.fromList (V.fromList <$> (fmap.fmap) fromChar ls)
      keyLocations = fmap head $ M.filter (not . null) $ (`gridFind` grid) <$> M.fromList [(a, KeySpace a) | a <- ['a'..'z']]
      emptyLocations = gridFind Empty grid
      doorLocations = concat $ (`gridFind` grid) <$> [Door a | a <- ['a'..'z']]
      nkeys = numKeys grid
      startPos = head $ gridFind Entrance grid
      --distanceMap = bestDistances (startPos:emptyLocations++(snd <$> M.toList keyLocations)++doorLocations) (snd <$> M.toList keyLocations) grid
      distanceMap = bestDistances startPos keyLocations grid
      explorer = Explorer grid startPos S.empty (NumSteps 0) S.empty distanceMap
  finalState <- stepExplorer M.empty keyLocations Dead nkeys (PQ.singleton 0 explorer)
  print finalState
