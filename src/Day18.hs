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
import qualified Data.Sequence as SQ
import qualified Data.Dequeue as DQ
import Data.IORef

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
data Explorer = Explorer (Int, Int) (S.Set Key) StepState deriving (Eq)

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

getNumSteps (NumSteps i) = i

type DistanceMap = M.Map ((Int, Int), (Int, Int)) Int
type KeyLocations = M.Map Char (Int, Int)

dbg = False

lockedDoor keys space = case space of
                          Door c -> not $ Key (toLower c) `S.member`  keys
                          _ -> False

-- The distance between two keys.
-- Need to better memoize this, since unlocking most doors doesn't change most of the values.
-- Store the doors we need to go past on the path too.
keyDistance :: SQ.Seq ((Int, Int), Int, S.Set Key, S.Set (Int, Int)) -> (Int, Int) -> Grid -> (Int, S.Set Key)
keyDistance queue (toX, toY) grid
  | SQ.null queue = error $ "didn't find path to " ++ show (toX, toY)
  | (x, y) == (toX, toY) = (distance, neededKeys)
  | otherwise = keyDistance nextQueue (toX, toY) grid
  where
    ((x, y), distance, neededKeys, seen) = SQ.index queue 0
    nrows = V.length grid
    ncols = V.length (grid V.! 0)
    currentSpace = grid V.! y V.! x
    newNeededKeys =
      if isDoor currentSpace then let (Door c) = currentSpace in S.insert (Key $ toLower c) neededKeys else neededKeys
    validNextPos (x,y) = x >= 0 && y >= 0 && x < ncols && y < nrows
                         && grid V.! y V.! x /= Wall
                         && not ((x,y) `S.member` seen)
    nextPos = filter validNextPos [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]
    nextStates = (, distance+1, newNeededKeys, S.insert (x, y) seen) <$> nextPos
    nextQueue = SQ.drop 1 queue SQ.>< SQ.fromList nextStates

-- The set of keys that we can get to from the current location.
-- We cache this per reachable position.
reachableKeys :: (Int, Int) -> [(Int, Int)] -> Grid -> KeyDoorCache
reachableKeys from keyLocations grid =
  M.filter ((>0) . fst)
  $ M.fromList [(to, keyDistance (SQ.singleton (from, 0, S.empty, S.empty)) to grid) | to <- keyLocations]

-- Cache (position,keyset) -> reachable keys
-- This will speed up the only actual costly thing
-- Key is position and keys, value is the reachable key map
type KeyDoorCache = M.Map (Int, Int) (Int, S.Set Key)
type OverallCache = M.Map (Int, Int, S.Set Key) (M.Map (Int, Int) Int)
type CostCache = M.Map (Int, Int, S.Set Key) StepState

-- TODO: sad times, cannot make use of the costcache in the DFS since we can't guarantee we ever saw the minimum
-- We need to be smarter e.g.
-- If we are at the finish, additional cost is zero
-- If we are not, then cost is smallest child 

-- CACHE needs to contain the score from here.

simpleDfs :: Int -> Grid -> IORef CostCache -> OverallCache -> M.Map (Int, Int) KeyDoorCache -> KeyLocations -> Int -> Explorer -> IO StepState
simpleDfs n grid costCacheRef keyCache cache keyLocations nkeys e = do
  costCache <- readIORef costCacheRef
  print costCache
  getLine

  let (Explorer (x, y) keys numSteps) = e
      cachedCost = M.lookup (x, y, keys) costCache
      currentSpace = grid V.! y V.! x
      nextKeys = case currentSpace of
                   (KeySpace c) -> S.insert (Key c) keys
                   _ -> keys

  if isJust cachedCost
     then do
       print "cache hit"
       return $ unsafeJ cachedCost
  else if S.size nextKeys == nkeys
    then do
      -- If we reached the end, then update the cache with the score if it was the best so far.
      costCache <- readIORef costCacheRef
      writeIORef costCacheRef $ M.insertWith min (x, y, nextKeys) (NumSteps 0) costCache
      return numSteps
    else do
      --if (n % 1000 == 0) then (print $ "States handled: " ++ show n) else return ()
      --print n
      --print numSteps
      when dbg $ do
        printGrid (x, y) grid
        print $ "Location: " ++ show (x, y)
        print $ "Keys: " ++ show nextKeys
        print $ "Current steps: " ++ show numSteps

      -- Filter down only to those keys whose paths are unlocked then drop the door information.
      -- This is lazy and won't happen in a cache hit
      let ffn (kx, ky) (_, need) =
            let (KeySpace c) = grid V.! ky V.! kx
             in need `S.isSubsetOf` nextKeys && not (Key c `S.member` nextKeys)
          rKeys' = fst <$> M.filterWithKey ffn (unsafeJ $ M.lookup (x, y) cache)
          cachedRKeys = M.lookup (x, y, nextKeys) keyCache
          -- Here we only lazily compute the expensive set intersection if cache miss
          rKeys = fromMaybe rKeys' cachedRKeys
          nextKeyCache =
            if isNothing cachedRKeys
               then M.insert (x, y, nextKeys) rKeys keyCache
               else keyCache
          rKeyList = M.toList rKeys
          nextStates =
            --(\(pos, d) -> Explorer pos nextKeys (NumSteps (d + getNumSteps numSteps)))
            (\(pos, d) -> Explorer pos nextKeys (NumSteps 0))
            <$> rKeyList

      when dbg $ do
        print $ "Key Cache length: " ++ show (M.size keyCache)
        print $ "Reachable keys: " ++ show rKeys
        _ <- if dbg then getLine else return ""
        return ()

      -- For each child, we need to see
      -- a) is it cached, and if so then get cached value
      -- b) if not cached, explore it and cache it, passing cache on to next child
      -- c) then take the minimum cost still, returning the updated cache

      -- First populate the cache from the children
      childCosts <- sequenceA $ (\(state@(Explorer (x,y) keys _), (_, d)) -> do
         -- For each child, get the now-memoized cost
         (NumSteps c) <- simpleDfs (n+1) grid costCacheRef nextKeyCache cache keyLocations nkeys state
         return $ NumSteps (d + c)) <$> (zip nextStates rKeyList)

      -- Get the best child cost
      let minCost = minimum childCosts

      -- Cache for further use if this is the best one.
      costCache <- readIORef costCacheRef
      writeIORef costCacheRef $ M.insertWith min (x, y, nextKeys) minCost costCache
         
      return minCost

-- Going back to a simple caching BFS.
-- TODO: bitstirng instead of set for efficient keying
-- TODO: We are still somehow missing a GLOBAL COST MAP. e.g. in position (x,y) with keys (ks) we could finish best in XXX steps. how to do this?
-- TODO: SCORE CACHING DFS. THIS HAS TO WORK, but hard to pass state around functionally without mutability
simpleBfs :: Int -> Grid -> OverallCache -> M.Map (Int, Int) KeyDoorCache -> KeyLocations -> Int -> DQ.BankersDequeue Explorer -> StepState -> S.Set (Int, Int, S.Set Key) -> IO StepState
simpleBfs n grid keyCache cache keyLocations nkeys queue stepState seenStates
  | DQ.null queue = return stepState
  | otherwise = do
  let Just (Explorer (x, y) keys numSteps, queueWithout) = DQ.popFront queue
      currentSpace = grid V.! y V.! x
      nextKeys = case currentSpace of
                   (KeySpace c) -> S.insert (Key c) keys
                   _ -> keys
      nextSeenStates = S.insert (x, y, nextKeys) seenStates
  if S.size nextKeys == nkeys
    then simpleBfs (n+1) grid keyCache cache keyLocations nkeys queueWithout (minimum [numSteps, stepState]) nextSeenStates
  else if numSteps > stepState
    then simpleBfs (n+1) grid keyCache cache keyLocations nkeys queueWithout stepState nextSeenStates
    else do
      print $ "Best: " ++ show stepState
      --if (n % 1000 == 0) then (print $ "States handled: " ++ show n) else return ()
      --print n
      --print numSteps
      when dbg $ do
        printGrid (x, y) grid
        print $ "Location: " ++ show (x, y)
        print $ "Keys: " ++ show nextKeys
        print $ "Current steps: " ++ show numSteps
      -- Filter down only to those keys whose paths are unlocked then drop the door information.
      -- This is lazy and won't happen in a cache hit
      let ffn (kx, ky) (_, need) =
            let (KeySpace c) = grid V.! ky V.! kx
             in need `S.isSubsetOf` nextKeys && not (Key c `S.member` nextKeys)
          rKeys' = fst <$> M.filterWithKey ffn (unsafeJ $ M.lookup (x, y) cache)
          cachedRKeys = M.lookup (x, y, nextKeys) keyCache
          -- Here we only lazily compute the expensive set intersection if cache miss
          rKeys = fromMaybe rKeys' cachedRKeys
          nextKeyCache =
            if isNothing cachedRKeys
               then M.insert (x, y, nextKeys) rKeys keyCache
               else keyCache
          rKeyList = M.toList rKeys
          nextStates =
            (\(pos, d) -> Explorer pos nextKeys (NumSteps (d + getNumSteps numSteps)))
            <$> rKeyList

          -- TODO: not using it
          --heuristic = sum (snd <$> rKeyList) -- sum of distances to all other keys

          -- TODO: currently doing DFS but need better for full grid
          --nextQueue = foldl' (\q st@(Explorer _ _ (NumSteps d)) -> PQ.insert ((-1)*n) st q) queueWithout nextStates
          --nextQueue = foldl' DQ.pushBack queueWithout nextStates
          
          -- Only push back unseen states
          nextStatesUnseen = filter (\(Explorer (x, y) keys _) -> not $ (x, y, keys) `S.member` seenStates) nextStates
          nextQueue = foldl' DQ.pushFront queueWithout nextStatesUnseen
       in do
         print $ length nextStatesUnseen
         when dbg $ do
           print $ "Cache length: " ++ show (M.size keyCache)
           print $ "Reachable keys: " ++ show rKeys
           print $ "Queue length: " ++ show (length nextQueue)
           --_ <- if dbg then getLine else return ""
           return ()
         simpleBfs (n+1) grid nextKeyCache cache keyLocations nkeys nextQueue stepState nextSeenStates

fullCache :: (Int, Int) -> [(Int, Int)] -> Grid -> M.Map (Int, Int) KeyDoorCache
fullCache startPos keyLocations grid =
  M.fromList
  $ (\p -> (p, reachableKeys p keyLocations grid)) <$> (startPos:keyLocations)

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
      explorer = Explorer startPos S.empty (NumSteps 0)
      cache = fullCache startPos (snd <$> M.toList keyLocations) grid
  --finalState <- simpleBfs 0 grid M.empty cache keyLocations nkeys (DQ.pushBack DQ.empty explorer) Dead S.empty
  costCacheRef <- newIORef M.empty
  finalState <- simpleDfs 0 grid costCacheRef M.empty cache keyLocations nkeys explorer
  print finalState
