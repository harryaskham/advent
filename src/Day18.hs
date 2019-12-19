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
import TwentyNineteen (unsafeJ)

data Space = Entrance | Empty | Door Char | KeySpace Char | Wall deriving (Eq)
type Grid = V.Vector (V.Vector Space)

instance Show Space where
  show Entrance = "@"
  show Empty = "."
  show (Door c) = [c]
  show (KeySpace c) = [c]
  show Wall = "#"

printGrid :: (Int, Int) -> Grid -> IO ()
printGrid (x, y) grid = sequenceA_ $ print . concat . V.toList <$> charGrid V.// [(y, updatedYth)]
  where
    charGrid = (fmap.fmap) show grid
    ythRow = charGrid V.! y
    updatedYth = ythRow V.// [(x, "*")]

isKey (KeySpace _) = True
isKey _ = False

gridFind :: Space -> Grid -> Maybe (Int, Int)
gridFind a grid = if null coords then Nothing else Just $ head coords
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
data Explorer = Explorer Grid (Int, Int) (S.Set Key) StepState (S.Set (Int, Int))

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
                   (Just k) -> vreplace2 k Empty grid
                   Nothing -> grid
    doorLoc = gridFind (Door $ toUpper c) grid
    withoutDoor = case doorLoc of
                    (Just k) -> vreplace2 k Empty withoutKey
                    Nothing -> withoutKey

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
overCap (NumSteps cap) (NumSteps now) = now > cap
overCap Dead _ = False


dbg = False

-- Okay, we added capping, didn't help
-- Maybe this needs to be a BFS... means that we find a single solution early
-- So that we can use this to cap a DFS

-- Returns the minimum number of steps to get all the keys.
stepExplorer :: StepState -> Int -> Explorer -> IO StepState
stepExplorer stepCap nkeys (Explorer grid (x, y) keys numSteps seenLocations) = do
  when dbg $ do
    printGrid (x, y) grid
    print $ "Location: " ++ show (x, y)
    print $ "Going next: " ++ show nextPos
    print $ "Current keys: " ++ show keys
    print $ "Current steps: " ++ show numSteps
    print $ "Current seen: " ++ show seenLocations

  --getLine
  print $ length keys
  -- If we found all the keys, finish
  if S.size nextKeys == nkeys
     then return numSteps
     -- Otherwise if we hit a dead end or we already found a nice branch, die
     else if null nextPos || overCap stepCap numSteps
     then return Dead
     -- Otherwise kick off a few branches.
     else do
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
  where
    -- If current space is key, pick it up, add to inventory, and unlock corresponding door.
    currentSpace = grid V.! y V.! x
    nextKeys = case currentSpace of
                 (KeySpace c) -> S.insert (Key c) keys
                 _ -> keys
    nextGrid = case currentSpace of
                 (KeySpace c) -> unlockDoor (Key c) grid
                 _ -> grid
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
  --ls <- lines <$> readFile "input/2019/18.txt"
  ls <- lines <$> readFile "input/2019/18_example.txt"
  let grid = V.fromList (V.fromList <$> (fmap.fmap) fromChar ls)
      nkeys = numKeys grid
      startPos = unsafeJ $ gridFind Entrance grid
      explorer = Explorer grid startPos S.empty (NumSteps 0) S.empty
  finalState <- stepExplorer Dead nkeys explorer
  print finalState
