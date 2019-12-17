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
data Space = Empty | Wall | Oxygen | Unknown | UpC | DownC | LeftC | RightC

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
        dim = 40
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

-- Strategy: proceed randomly to get idea of the space

runDroid :: Droid -> IO Droid
runDroid droid@(Droid machine grid pos@(x, y) facing) = do
  print droid
  getLine
  nextMachine <- stepUntilNOutputs 1 $ machine & inputs .~ [dirIn facing]
  let output = head $ nextMachine ^. outputs
      updatedCoord = case facing of
                       North -> (x, y-1)
                       South -> (x, y+1)
                       East -> (x+1, y)
                       West -> (x-1, y)
      nextPos = case output of
                  0 -> pos
                  _ -> updatedCoord
      nextGrid = case output of
                   0 -> M.insert updatedCoord Wall grid
                   1 -> M.insert updatedCoord Empty grid
                   2 -> M.insert updatedCoord Oxygen grid
      nextFacing = case output of
                     0 -> case facing of
                            East -> North
                            North -> West
                            West -> South
                            South -> East
                     _ -> facing
  runDroid $ Droid (nextMachine & outputs .~ []) nextGrid nextPos nextFacing

day15 :: IO ()
day15 = do
  program <- readProgram "input/2019/15.txt"
  let droid = Droid (Machine 0 [] [] program 0) M.empty (0, 0) North
  runDroid droid
  return ()
