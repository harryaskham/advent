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
import Control.Monad.Search

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
data Explorer = Explorer Grid (Int, Int) (S.Set Key) StepState

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

-- An A* search heuristic.
heuristic :: DistanceMap -> M.Map Char (Int, Int) -> Explorer -> Int
heuristic distanceMap keyLocations (Explorer _ pos keys numSteps) =
  sum $ dist <$> ZipList remainingKeyLocs <*> ZipList (drop 1 remainingKeyLocs)
  --sum $ dist pos <$> remainingKeyLocs
  where
    dist :: (Int, Int) -> (Int, Int) -> Int
    dist from to = unsafeJ $ M.lookup (from,to) distanceMap
    remainingKeyLocs :: [(Int, Int)]
    remainingKeyLocs = snd <$> M.toList (foldl' (\acc (Key k) -> M.delete k acc) keyLocations (S.toList keys))

-- Get the current distances to all keys cache.
-- This is a cache keyed by point, storing its distance to all keys
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

-- Given the origin and key locations, find the no-doors distances between all things.
bestDistances :: (Int, Int) -> M.Map Char (Int, Int) -> Grid -> DistanceMap
bestDistances origin keyLocations grid =
  M.fromList [((from,to), bestDistance from to grid S.empty) | from <- froms, to <- tos]
  where
    tos = snd <$> M.toList keyLocations
    froms = origin:tos

getNumSteps (NumSteps i) = i

type DistanceMap = M.Map ((Int, Int), (Int, Int)) Int
type KeyLocations = M.Map Char (Int, Int)

dbg = True

-- The distance between two keys.
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

-- The set of keys that we can get to from the current location.
-- We cache this per reachable position.
reachableKeys :: S.Set Key -> (Int, Int) -> KeyLocations -> Grid -> M.Map (Int, Int) Int
reachableKeys keys from keyLocations grid =
  unsafeJ <$> M.filter (> Just 0) (M.fromList [(to, keyDistance from to grid S.empty) | to <- remainingKeyLocs])
  where
    remainingKeyLocs :: [(Int, Int)]
    remainingKeyLocs = snd <$> M.toList (foldl' (\acc (Key k) -> M.delete k acc) keyLocations (S.toList keys))

-- Cache (position,keyset) -> reachable keys
-- This will speed up the only actual costly thing
-- Key is position and keys, value is the reachable key map
type Cache = M.Map ((Int, Int), S.Set Key) (M.Map (Int, Int) Int)

-- Cache the best cost from a given point.
type CostCache = M.Map ((Int, Int), S.Set Key) StepState

-- Going back to a simple caching BFS.
simpleBfs :: Cache -> KeyLocations -> StepState -> Int -> [Explorer] -> IO StepState
simpleBfs _ _ stepCap _ [] = return stepCap
simpleBfs cache keyLocations stepCap nkeys (e:rest) = do
  let (Explorer grid (x, y) keys numSteps) = e
      currentSpace = grid V.! y V.! x
      nextKeys = case currentSpace of
                   (KeySpace c) -> S.insert (Key c) keys
                   _ -> keys
      nextGrid = case currentSpace of
                   (KeySpace c) -> unlockDoor (Key c) grid
                   _ -> grid

  if S.size nextKeys == nkeys
     then return numSteps
     else do
       let cachedRKeys = M.lookup ((x,y), keys) cache
           rKeys = fromMaybe (reachableKeys keys (x, y) keyLocations nextGrid) cachedRKeys
           nextCache = M.insert ((x,y),keys) rKeys cache
           nextStates =
             (\(pos, d) -> Explorer nextGrid pos nextKeys (NumSteps (d + getNumSteps numSteps)))
             <$> M.toList rKeys
           nextQueue = rest ++ nextStates
           sortedQueue = sortOn (\(Explorer _ _ _ (NumSteps d)) -> d) nextQueue
       --if isJust cachedRKeys then print "Reachable Cache hit" else print "Reachable Cache miss"
       simpleBfs nextCache keyLocations stepCap nkeys sortedQueue



           



-- Returns the minimum number of steps to get all the keys.
stepExplorer :: DistanceMap -> Cache -> KeyLocations -> StepState -> Int -> PQ.MinPQueue Int Explorer -> IO StepState
stepExplorer distanceMap cache keyLocations stepCap nkeys queue
  | PQ.null queue = return stepCap
  | otherwise = do
  when dbg $ do
    printGrid (x, y) grid
    print $ "Location: " ++ show (x, y)
    print $ "NumKeys: " ++ show (length keys)
    print $ "Current steps: " ++ show numSteps
    print $ "Best: " ++ show stepCap
  if S.size nextKeys == nkeys
     then do
       let newCap = minimum [stepCap, numSteps]
       print newCap
       stepExplorer distanceMap cache keyLocations newCap nkeys queueWithoutCurrent
     else 
       let rKeys = fromMaybe (reachableKeys keys (x, y) keyLocations nextGrid) $ M.lookup ((x,y),keys) cache
           nextCache = M.insert ((x,y),keys) rKeys cache
           nextStates =
             (\(pos, d) -> Explorer nextGrid pos nextKeys (NumSteps (d + getNumSteps numSteps)))
             <$> M.toList rKeys
           nextQueue =
             foldl'
             (\q ex@(Explorer _ _ _ (NumSteps st)) -> PQ.insert (st + heuristic distanceMap keyLocations ex) ex q)
             queueWithoutCurrent nextStates
        in do
          when dbg $ do
            print $ "Cache length: " ++ show (M.size cache)
            print $ "Reachable keys: " ++ show rKeys
            let (_,nxt) = PQ.findMin nextQueue
            print $ "Heuristic: " ++ show (heuristic distanceMap keyLocations nxt)
            _ <- if dbg then getLine else return ""
            return ()
          stepExplorer distanceMap nextCache keyLocations stepCap nkeys nextQueue
  where
    ((_, Explorer grid (x, y) keys numSteps), queueWithoutCurrent) = PQ.deleteFindMin queue
    currentSpace = grid V.! y V.! x
    nextKeys = case currentSpace of
                 (KeySpace c) -> S.insert (Key c) keys
                 _ -> keys
    nextGrid = case currentSpace of
                 (KeySpace c) -> unlockDoor (Key c) grid
                 _ -> grid

day18 :: IO ()
day18 = do
  --ls <- lines <$> readFile "input/2019/18.txt"
  ls <- lines <$> readFile "input/2019/18_example.txt"
  let grid = V.fromList (V.fromList <$> (fmap.fmap) fromChar ls)
      keyLocations =
        fmap head 
        $ M.filter (not . null)
        $ (`gridFind` grid)
        <$> M.fromList [(a, KeySpace a) | a <- ['a'..'z']]
      nkeys = numKeys grid
      startPos = head $ gridFind Entrance grid
      distanceMap = bestDistances startPos keyLocations grid
      explorer = Explorer grid startPos S.empty (NumSteps 0)
  --finalState <- stepExplorer distanceMap M.empty keyLocations Dead nkeys (PQ.singleton 0 explorer)
  finalState <- simpleBfs M.empty keyLocations Dead nkeys [Explorer grid startPos S.empty (NumSteps 0)]
  print finalState
