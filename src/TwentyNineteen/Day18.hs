{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module TwentyNineteen.Day18 where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Lens hiding (Empty)
import Control.Monad
import Control.Monad.Search
import Coord
import Data.Char
import qualified Data.Dequeue as DQ
import Data.Foldable
import Data.Function ((&))
import Data.IORef
import Data.List
import qualified Data.List.Safe as LS
import Data.List.Split hiding (condense)
import qualified Data.Map.Strict as M
import qualified Data.Matrix as MX
import Data.Maybe
import Data.Ord
import qualified Data.PQueue.Prio.Min as PQ
import Data.Ratio
import qualified Data.Sequence as SQ
import qualified Data.Set as S
import qualified Data.Tree as T
import qualified Data.Vector as V
import qualified Data.Vector.Split as VS
import Debug.Trace
import System.IO
import System.IO.HiddenChar
import System.Random
import Text.ParserCombinators.ReadP
import Util

data Space = Entrance | Empty | Door Char | KeySpace Char | Wall deriving (Eq)

type Grid = V.Vector (V.Vector Space)

instance Show Space where
  show Entrance = "."
  show Empty = "."
  show (Door c) = [c]
  show (KeySpace c) = [c]
  show Wall = "#"

printGrid :: [(Int, Int)] -> Grid -> IO ()
printGrid positions grid = sequenceA_ $ print . concat . V.toList <$> withStarts
  where
    charGrid = (fmap . fmap) show grid
    withStarts = foldl' (\g pos -> vreplace2 pos "@" g) charGrid positions

isKey (KeySpace _) = True
isKey _ = False

gridFind :: Space -> Grid -> [(Int, Int)]
gridFind a grid = coords
  where
    nrows = V.length grid
    ncols = V.length (grid V.! 0)
    coords = [(x, y) | x <- [0 .. ncols -1], y <- [0 .. nrows -1], grid V.! y V.! x == a]

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

isDoor :: Space -> Bool
isDoor space = case space of
  (Door _) -> True
  _ -> False

numKeys :: Grid -> Int
numKeys grid = length [(x, y) | x <- [0 .. ncols -1], y <- [0 .. nrows -1], isKey (grid V.! y V.! x)]
  where
    nrows = V.length grid
    ncols = V.length (grid V.! 0)

getNumSteps (NumSteps i) = i
getNumSteps Dead = 10000000 -- TODO bad hack

type DistanceMap = M.Map ((Int, Int), (Int, Int)) Int

type KeyLocations = M.Map Char (Int, Int)

dbg = True

-- The distance between two keys.
-- Need to better memoize this, since unlocking most doors doesn't change most of the values.
-- Store the doors we need to go past on the path too.
keyDistance :: SQ.Seq ((Int, Int), Int, S.Set Key, S.Set (Int, Int)) -> (Int, Int) -> Grid -> Maybe (Int, S.Set Key)
keyDistance queue (toX, toY) grid
  | SQ.null queue = Nothing
  | (x, y) == (toX, toY) = Just (distance, neededKeys)
  | otherwise = keyDistance nextQueue (toX, toY) grid
  where
    ((x, y), distance, neededKeys, seen) = SQ.index queue 0
    nrows = V.length grid
    ncols = V.length (grid V.! 0)
    currentSpace = grid V.! y V.! x
    newNeededKeys =
      if isDoor currentSpace
        then let (Door c) = currentSpace in S.insert (Key $ toLower c) neededKeys
        else neededKeys
    validNextPos (x, y) =
      x >= 0 && y >= 0 && x < ncols && y < nrows
        && grid V.! y V.! x /= Wall
        && not ((x, y) `S.member` seen)
    nextPos = filter validNextPos [(x + 1, y), (x -1, y), (x, y + 1), (x, y -1)]
    nextStates = (,distance + 1,newNeededKeys,S.insert (x, y) seen) <$> nextPos
    nextQueue = SQ.drop 1 queue SQ.>< SQ.fromList nextStates

-- The set of keys that we can get to from the current location.
-- We cache this per reachable position.
reachableKeys :: (Int, Int) -> [(Int, Int)] -> Grid -> KeyDoorCache
reachableKeys from keyLocations grid =
  M.filter ((> 0) . fst) $
    M.fromList $
      (\(t, d) -> (t, unjust d))
        <$> filter
          (isJust . snd)
          [(to, keyDistance (SQ.singleton (from, 0, S.empty, S.empty)) to grid) | to <- keyLocations]

-- From a given position, how far away are each key and which keys are needed?
type KeyDoorCache = M.Map (Int, Int) (Int, S.Set Key)

-- For a given position, how far is the best distance to get to all keys?
type CostCache = M.Map (Int, Int, S.Set Key) StepState

runDFS :: Int -> Grid -> IORef CostCache -> IORef StepState -> M.Map (Int, Int) KeyDoorCache -> Int -> Explorer -> IO StepState
runDFS n grid costCacheRef bestSoFarRef cache nkeys e = do
  -- Read the global DFS state first.
  costCache <- readIORef costCacheRef
  bestSoFar <- readIORef bestSoFarRef

  let (Explorer (x, y) keys numSteps) = e
      currentSpace = grid V.! y V.! x
      nextKeys = case currentSpace of
        (KeySpace c) -> S.insert (Key c) keys
        _ -> keys
      cachedCost = M.lookup (x, y, nextKeys) costCache

  -- If we cached this location we can just return its cost, trusting we DFS'd properly first time
  if isJust cachedCost
    then return $ unjust cachedCost
    else -- If we are already greater than our largest path, terminate early.

      if numSteps > bestSoFar
        then return Dead
        else -- If we found all the keys, track the best path and return zero.

          if S.size nextKeys == nkeys
            then do
              when (numSteps < bestSoFar) $ writeIORef bestSoFarRef numSteps
              return $ NumSteps 0
            else -- Otherwise run the proper DFS.
            do
              when dbg $ do
                printGrid [(x, y)] grid
                print $ "Location: " ++ show (x, y)
                print $ "Keys: " ++ show nextKeys
                print $ "Current steps: " ++ show numSteps

              -- Filter down only to those keys we don't have, whose paths are unlocked then drop the door information.
              let filterAccessibleKeys (kx, ky) (_, need) =
                    let (KeySpace c) = grid V.! ky V.! kx
                     in need `S.isSubsetOf` nextKeys && not (Key c `S.member` nextKeys)
                  rKeys = fst <$> M.filterWithKey filterAccessibleKeys (unjust $ M.lookup (x, y) cache)
                  rKeyList = M.toList rKeys
                  -- Create next states for all reachable keys to explore.
                  nextStates =
                    (\(pos, d) -> Explorer pos nextKeys (NumSteps (d + getNumSteps numSteps)))
                      <$> rKeyList

              when dbg $ do
                print $ "Reachable keys: " ++ show rKeys
                _ <- if dbg then getLine else return ""
                return ()

              -- Get all child costs, either cached or computed.
              childCosts <-
                sequenceA $
                  runDFS (n + 1) grid costCacheRef bestSoFarRef cache nkeys
                    <$> nextStates

              -- Get the best child cost, taking distance-to-child into account.
              let childDistances = snd <$> rKeyList
              let minCost =
                    minimum . getZipList $
                      (+)
                        <$> ZipList (getNumSteps <$> childCosts)
                        <*> ZipList childDistances

              -- Cache, because the DFS guarantees this was the best from this position.
              modifyIORef' costCacheRef $ M.insert (x, y, nextKeys) (NumSteps minCost)

              return (NumSteps minCost)

-- Creates a cache from position to the key-distance cache.
fullCache :: [(Int, Int)] -> [(Int, Int)] -> Grid -> M.Map (Int, Int) KeyDoorCache
fullCache starts keyLocations grid =
  M.fromList $
    (\p -> (p, reachableKeys p keyLocations grid)) <$> (starts ++ keyLocations)

day18_1 :: IO ()
day18_1 = do
  ls <- lines <$> readFile "input/2019/18.txt"
  let grid = V.fromList (V.fromList <$> (fmap . fmap) fromChar ls)
      keyLocations =
        fmap head $
          M.filter (not . null) $
            (`gridFind` grid)
              <$> M.fromList [(a, KeySpace a) | a <- ['a' .. 'z']]
      nkeys = numKeys grid
      startPos = head $ gridFind Entrance grid
      explorer = Explorer startPos S.empty (NumSteps 0)
      cache = fullCache [startPos] (snd <$> M.toList keyLocations) grid
  costCacheRef <- newIORef M.empty
  bestSoFarRef <- newIORef Dead
  finalState <- runDFS 0 grid costCacheRef bestSoFarRef cache nkeys explorer
  print finalState

chopGrid :: Int -> Int -> Int -> Int -> Grid -> Grid
chopGrid startX lenX startY lenY grid = choppedGrid
  where
    choppedRows = V.slice startY lenY grid
    choppedGrid = V.slice startX lenX <$> choppedRows

vreplace2 :: (Int, Int) -> a -> V.Vector (V.Vector a) -> V.Vector (V.Vector a)
vreplace2 (x, y) a g = g V.// [(y, row)]
  where
    row = (g V.! y) V.// [(x, a)]

-- Get rid of any doors that don't have keys in the grid.
removeDoorsWithoutKeys :: Grid -> Grid
removeDoorsWithoutKeys grid =
  M.foldlWithKey'
    ( \g doorC loc ->
        if not $ doorC `S.member` doorsToKeep
          then vreplace2 loc Empty g
          else g
    )
    grid
    doorLocations
  where
    doorsToKeep =
      S.fromList $
        fmap fst $
          filter (not . null . snd) $
            zip ['A' .. 'Z'] ((`gridFind` grid) <$> [KeySpace c | c <- ['a' .. 'z']])
    doorLocations =
      fmap head $
        M.filter (not . null) $
          (`gridFind` grid)
            <$> M.fromList [(a, Door a) | a <- ['A' .. 'Z']]

removeAllDoors :: Grid -> Grid
removeAllDoors grid = M.foldl' (\g loc -> vreplace2 loc Empty g) grid doorLocations
  where
    doorLocations =
      fmap head $
        M.filter (not . null) $
          (`gridFind` grid)
            <$> M.fromList [(a, Door a) | a <- ['A' .. 'Z']]

-- Stores state of the robots
data Explorers = Explorers [(Int, Int)] (S.Set Key) StepState deriving (Eq)

type CostCache' = M.Map (Int, Int, Int, Int, Int, Int, Int, Int, S.Set Key) StepState

runDFS' :: Int -> Grid -> IORef CostCache' -> IORef StepState -> M.Map (Int, Int) KeyDoorCache -> Int -> Explorers -> IO StepState
runDFS' n grid costCacheRef bestSoFarRef cache nkeys e = do
  -- Read the global DFS state first.
  costCache <- readIORef costCacheRef
  bestSoFar <- readIORef bestSoFarRef

  let (Explorers positions keys numSteps) = e
      currentSpaces = (\(x, y) -> grid V.! y V.! x) <$> positions
      nextKeys =
        maximumBy (comparing S.size) $
          ( \case
              (KeySpace c) -> S.insert (Key c) keys
              _ -> keys
          )
            <$> currentSpaces
      cacheKey =
        ( fst $ positions !! 0,
          snd $ positions !! 0,
          fst $ positions !! 1,
          snd $ positions !! 1,
          fst $ positions !! 2,
          snd $ positions !! 2,
          fst $ positions !! 3,
          snd $ positions !! 3,
          nextKeys
        )
      cachedCost = M.lookup cacheKey costCache

  -- If we cached this location we can just return its cost, trusting we DFS'd properly first time
  if isJust cachedCost
    then return $ unjust cachedCost
    else -- If we are already greater than our largest path, terminate early.

      if numSteps > bestSoFar
        then return Dead
        else -- If we found all the keys, track the best path and return zero.

          if S.size nextKeys == nkeys
            then do
              when (numSteps < bestSoFar) $ writeIORef bestSoFarRef numSteps
              return $ NumSteps 0
            else -- Otherwise run the proper DFS.
            do
              when dbg $ do
                printGrid positions grid
                print $ "Location: " ++ show positions
                print $ "Keys: " ++ show nextKeys
                print $ "Current steps: " ++ show numSteps

              -- Our options are the moves available to any given robot
              -- Filter down only to those keys we don't have, whose paths are unlocked then drop the door information.
              let filterAccessibleKeys (kx, ky) (_, need) =
                    let (KeySpace c) = grid V.! ky V.! kx
                     in need `S.isSubsetOf` nextKeys && not (Key c `S.member` nextKeys)
                  -- Per-robot reachable keys
                  rKeys = (\pos -> fst <$> M.filterWithKey filterAccessibleKeys (unjust $ M.lookup pos cache)) <$> positions
                  rKeyList = M.toList <$> rKeys
                  -- Create next states for all reachable keys to explore.
                  -- We need to create all combinations of positions with each element swapped out for corresponding rKeysList
                  nextPosDs :: [([(Int, Int)], Int)]
                  nextPosDs = concat [(\(pos, d) -> (positions & ix i .~ pos, d)) <$> (rKeyList !! i) | i <- [0 .. length positions - 1]]
                  -- Use these to generate all possible states.
                  nextStates :: [Explorers]
                  nextStates =
                    (\(nextPositions, d) -> Explorers nextPositions nextKeys (NumSteps (d + getNumSteps numSteps)))
                      <$> nextPosDs

              when dbg $ do
                print $ "Considering num next states: " ++ show (length nextStates)
                print $ "Reachable keys: " ++ show rKeys
                _ <- if dbg then getLine else return ""
                return ()

              -- Get all child costs, either cached or computed.
              childCosts <-
                sequenceA $
                  runDFS' (n + 1) grid costCacheRef bestSoFarRef cache nkeys
                    <$> nextStates

              -- Get the best child cost, taking distance-to-child into account.
              let childDistances = snd <$> nextPosDs
              let minCost =
                    minimum . getZipList $
                      (+)
                        <$> ZipList (getNumSteps <$> childCosts)
                        <*> ZipList childDistances

              -- Cache, because the DFS guarantees this was the best from this position.
              modifyIORef' costCacheRef $ M.insert cacheKey (NumSteps minCost)

              return (NumSteps minCost)

day18_2 :: IO ()
day18_2 = do
  --ls <- lines <$> readFile "input/2019/18_2_cut.txt"
  ls <- lines <$> readFile "input/2019/18_2.txt"
  --ls <- lines <$> readFile "input/2019/18_2_example.txt"
  let grid = V.fromList (V.fromList <$> (fmap . fmap) fromChar ls)
      keyLocations =
        fmap head $
          M.filter (not . null) $
            (`gridFind` grid)
              <$> M.fromList [(a, KeySpace a) | a <- ['a' .. 'z']]
      nkeys = numKeys grid
      starts = gridFind Entrance grid
      explorers = Explorers starts S.empty (NumSteps 0)
      cache = fullCache starts (snd <$> M.toList keyLocations) grid
  costCacheRef <- newIORef M.empty
  bestSoFarRef <- newIORef Dead
  finalState <- runDFS' 0 grid costCacheRef bestSoFarRef cache nkeys explorers
  print finalState

-- Okay, weird - even with NO DOORS the shortest path is lower than reference value.
-- Let alone without the doors we can't open from each quadrant
-- Aiming for 1940 but getting 1976 with no doors

day18_cheat :: IO ()
day18_cheat = do
  ls <- lines <$> readFile "input/2019/18_2.txt"
  let grid = V.fromList (V.fromList <$> (fmap . fmap) fromChar ls)
      grid1 = chopGrid 0 41 0 41 grid
      grid2 = chopGrid 40 41 0 41 grid
      grid3 = chopGrid 0 41 40 41 grid
      grid4 = chopGrid 40 41 40 41 grid
      --grids = removeAllDoors <$> [grid1, grid2, grid3, grid4]
      grids = removeDoorsWithoutKeys <$> [grid1, grid2, grid3, grid4]
      solveGrid g =
        let keyLocations =
              fmap head $
                M.filter (not . null) $
                  (`gridFind` g)
                    <$> M.fromList [(a, KeySpace a) | a <- ['a' .. 'z']]
            nkeys = numKeys g
            startPos = head $ gridFind Entrance g
            explorer = Explorer startPos S.empty (NumSteps 0)
            cache = fullCache [startPos] (snd <$> M.toList keyLocations) g
         in do
              costCacheRef <- newIORef M.empty
              bestSoFarRef <- newIORef Dead
              runDFS 0 g costCacheRef bestSoFarRef cache nkeys explorer
  scores <- sequenceA $ solveGrid <$> grids
  print scores
  print $ sum $ getNumSteps <$> scores
