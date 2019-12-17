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
        dim = 60
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

undoUntilUnstuck :: MoveStack -> Droid -> IO Droid
undoUntilUnstuck [] droid = runDroid [] droid
undoUntilUnstuck (m:stack) droid@(Droid machine grid pos _) = do
  -- Undo the top move on the stack
  let move = case m of
               North -> South
               East -> West
               South -> North
               West -> East
  (movedDroid, _) <- oneMove move [] droid
  -- Check whether there is anything unexplored and if so, we're good
  let (Droid _ grid (x, y) facing) = rotateUntilGood movedDroid
  if isNothing $ M.lookup (updatedCoord facing x y) grid
     then runDroid stack movedDroid
     else undoUntilUnstuck stack movedDroid

oneMove :: Dir -> MoveStack -> Droid -> IO (Droid, MoveStack)
oneMove dir stack droid@(Droid machine grid pos@(x, y) facing) = do
  nextMachine <- stepUntilNOutputs 1 $ machine & inputs .~ [dirIn dir]
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
                    _ -> dir:stack
  if output == 2
     then print ("Found oxygen at " ++ show x ++ " " ++ show y) >> return (droid, stack)
     else return (Droid (nextMachine & outputs .~ []) nextGrid nextPos facing, nextStack)


runDroid :: MoveStack -> Droid -> IO Droid
runDroid stack droid' = do
  print droid'
  getLine
  if stuck droid'
     then undoUntilUnstuck stack droid'
     else do
       -- Shadow droid, and ensure we are always facing the right way.
       let droid@(Droid machine grid pos@(x, y) facing) = rotateUntilGood droid'
       -- Otherwise, step forwards in the direction we are facing.
       (movedDroid, nextStack) <- oneMove facing stack droid
       runDroid nextStack movedDroid

day15 :: IO ()
day15 = do
  program <- readProgram "input/2019/15.txt"
  let droid = Droid (Machine 0 [] [] program 0) M.empty (0, 0) North
  runDroid [] droid
  return ()
