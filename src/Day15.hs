{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiWayIf #-}

module Day15 where

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
import TwentyNineteen (Machine(..), readProgram, stepUntilNOutputs, inputs, outputs, clear)

data Dir = North | South | West | East deriving (Show, Enum)
data Space = Empty | Wall | Oxygen | Unknown | UpC | DownC | LeftC | RightC deriving (Eq)

instance Show Space where
  show Empty = "."
  show Wall = "#"
  show Oxygen = "!"
  show Unknown = " "
  show UpC = "^"
  show DownC = "v"
  show LeftC = "<"
  show RightC = ">"

type Grid = M.Map (Int, Int) Space
type Pos = (Int, Int)
data Droid = Droid Machine Grid Pos Dir

-- Always hug left wall? e.g. go straight, take right if we can't take left, take 

instance Show Droid where
  show (Droid _ grid (x, y) facing) =
    MX.prettyMatrix $ MX.matrix dim dim drawFn
      where
        dim = 50
        drawFn (y', x') = if (x, y) == (x'-(dim `div` 2), y'-(dim `div` 2))
                             then case facing of
                                    North -> UpC
                                    East -> RightC
                                    South -> DownC
                                    West -> LeftC
                              else fromMaybe Unknown $ M.lookup (x'-(dim `div` 2), y'-(dim `div` 2)) grid

dirIn :: Dir -> Integer
dirIn North = 1
dirIn South = 2
dirIn West = 3
dirIn East = 4

rotate :: Dir -> Dir
rotate West = North
rotate North = East
rotate East = South
rotate South = West

updatedCoord :: Dir -> Int -> Int -> (Int, Int)
updatedCoord facing x y = case facing of
                            North -> (x, y-1)
                            South -> (x, y+1)
                            East -> (x+1, y)
                            West -> (x-1, y)

-- Rotate the droid until it's facing either an unknown square (preferred), or otherwise an empty one.
rotateUntilGood :: Droid -> Droid
rotateUntilGood droid@(Droid machine grid pos@(x, y) facing) =
  if not . null $ nextUnexplored
     then Droid machine grid pos $ fst . head $ nextUnexplored
     else Droid machine grid pos $ fst . head $ nextEmpty
  where
    -- Ordered this way so that we prefer the fewest rotations where possible.
    -- Will always prioritise rotating rather than going straight, hopefully letting us do an always-turn-right policy.
    dirs = [rotate facing, rotate.rotate.rotate $ facing, facing, rotate.rotate $ facing]
    nextCoords = updatedCoord <$> dirs <*> [x] <*> [y]
    nextSpaces = M.lookup <$> nextCoords <*> pure grid
    nextUnexplored = filter (\(d,s) -> isNothing s) $ zip dirs nextSpaces 
    nextEmpty = filter (\(d,s) -> s == Just Empty) $ zip dirs nextSpaces

-- Keep a move-stack, and if we hit a dead end then undo the moves until we hit something with space, then follow adding to the movestack again.

type MoveStack = [Dir]

stuck :: Droid -> Bool
stuck (Droid _ grid (x, y) facing) = length surroundingWalls > 2
  where
    dirs = [rotate facing, rotate.rotate.rotate $ facing, facing, rotate.rotate $ facing]
    nextCoords = updatedCoord <$> dirs <*> [x] <*> [y]
    surroundingWalls = filter (== Just Wall) (M.lookup <$> nextCoords <*> pure grid)

undoUntilUnstuck :: Int -> MoveStack -> Droid -> IO Droid
undoUntilUnstuck n [] droid = runDroid n [] droid
undoUntilUnstuck n (m:stack) droid@(Droid machine grid pos _) = do
  --print (m:stack)
  --print droid
  print $ length (m:stack)
  --getLine
  -- Undo the top move on the stack
  let move = case m of
               North -> South
               East -> West
               South -> North
               West -> East
  -- print $ "Undoing move: " ++ show m ++ " by going " ++ show move
  (movedDroid, _) <- oneMove [] (Droid machine grid pos move)
  -- Check whether there is anything unexplored and if so, we're good
  let (Droid _ grid (x, y) facing) = rotateUntilGood movedDroid
  if isNothing $ M.lookup (updatedCoord facing x y) grid
     then runDroid (n+1) stack movedDroid
     else undoUntilUnstuck (n+1) stack movedDroid

oneMove :: MoveStack -> Droid -> IO (Droid, MoveStack)
oneMove stack droid@(Droid machine grid pos@(x, y) facing) = do
  nextMachine <- stepUntilNOutputs 1 $ machine & inputs .~ [dirIn facing]
  let output = head $ nextMachine ^. outputs
      nextCoord = updatedCoord facing x y
      nextPos = case output of
                  0 -> pos
                  _ -> nextCoord
      nextGrid = case output of
                   0 -> M.insert nextCoord Wall grid
                   1 -> M.insert nextCoord Empty grid
                   2 -> M.insert nextCoord Oxygen grid
      nextStack = case output of
                    0 -> stack
                    _ -> facing:stack
  when (output == 2) $ print ("Found oxygen at " ++ show x ++ " " ++ show y)
  return (Droid (nextMachine & outputs .~ []) nextGrid nextPos facing, nextStack)

-- Run droid until grid is complete.
runDroid :: Int -> MoveStack -> Droid -> IO Droid
runDroid n stack droid' = do
  --print droid'
  --getLine
  --print n
  print $ length stack
  if n > 10000 && null stack
     then return droid'
     else if stuck droid'
             then undoUntilUnstuck n stack droid'
             else do
               -- Shadow droid, and ensure we are always facing the right way.
               let droid@(Droid machine grid pos@(x, y) facing) = rotateUntilGood droid'
               -- Otherwise, step forwards in the direction we are facing.
               (movedDroid@(Droid _ grid' _ _), nextStack) <- oneMove stack droid
               runDroid (n+1) nextStack movedDroid

-- Oxygen was at -17, -20, stack size of 295+1 when getting there

neighbours :: MX.Matrix Space -> (Int, Int) -> [Maybe Space]
neighbours orig (x, y) = [ MX.safeGet y (x+1) orig
                         , MX.safeGet y (x-1) orig
                         , MX.safeGet (y+1) x orig
                         , MX.safeGet (y-1) x orig ]

floodPass :: MX.Matrix Space -> MX.Matrix Space
floodPass orig = MX.matrix (MX.nrows orig) (MX.ncols orig) fillCoord
  where
    fillCoord (y, x) = case MX.safeGet y x orig of
                         Just Wall -> Wall
                         Just Oxygen -> Oxygen
                         Just Empty -> if Just Oxygen `elem` neighbours orig (x, y) then Oxygen else Empty
                         Just Unknown -> Unknown
                         Nothing -> Unknown

gridToMatrix grid = MX.matrix dim dim drawFn
  where
    dim = 45
    drawFn (y', x') = fromMaybe Unknown $ M.lookup (x'-(dim `div` 2), y'-(dim `div` 2)) grid

floodUntilComplete :: Int -> MX.Matrix Space -> Int
floodUntilComplete n m =
  if m == flooded
     then n
     else floodUntilComplete (n+1) flooded
  where
    flooded = floodPass m

day15 :: IO ()
day15 = do
  program <- readProgram "input/2019/15.txt"
  let droid = Droid (Machine 0 [] [] program 0) M.empty (0, 0) North
  droid'@(Droid _ completeGrid _ _) <- runDroid 0 [] droid
  let matrix = gridToMatrix completeGrid
  print $ floodUntilComplete 0 matrix
