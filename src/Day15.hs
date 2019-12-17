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
    dirs = [facing, rotate facing, rotate.rotate $ facing, rotate.rotate.rotate $ facing]
    nextCoords = updatedCoord <$> dirs <*> [x] <*> [y]
    nextSpaces = M.lookup <$> nextCoords <*> pure grid
    nextUnexplored = filter (\(d,s) -> isNothing s) $ zip dirs nextSpaces 
    nextEmpty = filter (\(d,s) -> s == Just Empty) $ zip dirs nextSpaces 


runDroid :: Droid -> IO Droid
runDroid droid@(Droid machine grid pos@(x, y) facing) = do
  --print droid
  --getLine
  let nextCoord = updatedCoord facing x y
  -- If we are facing a wall as far as we know, then just immediately rotate.
  if M.lookup nextCoord grid == Just Wall
     then runDroid (rotateUntilGood droid)
     else do
       -- Otherwise, step forwards in the direction we are facing.
       nextMachine <- stepUntilNOutputs 1 $ machine & inputs .~ [dirIn facing]
       let output = head $ nextMachine ^. outputs
           nextPos = case output of
                       0 -> pos
                       _ -> nextCoord
           nextGrid = case output of
                        0 -> M.insert nextCoord Wall grid
                        1 -> M.insert nextCoord Empty grid
                        2 -> M.insert nextCoord Oxygen grid
       if output == 2
          then print ("Found oxygen at " ++ show x ++ " " ++ show y) >> return droid
          else runDroid $ Droid (nextMachine & outputs .~ []) nextGrid nextPos facing

day15 :: IO ()
day15 = do
  program <- readProgram "input/2019/15.txt"
  let droid = Droid (Machine 0 [] [] program 0) M.empty (0, 0) North
  runDroid droid
  return ()
