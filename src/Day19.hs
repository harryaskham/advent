{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiWayIf #-}

module Day19 where

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
import TwentyNineteen (Machine(..), readProgram, stepUntilNOutputs, inputs, outputs, clear, runProgram, unsafeJ)

day19_1 :: IO ()
day19_1 = do
  program <- readProgram "input/2019/19.txt"
  let inputs = [[x, y] | x <- [0..49], y <- [0..49]]
      machines = [Machine 0 i [] program 0 | i <- inputs]
  machines' <- sequenceA $ runProgram <$> machines
  print $ sum $ concat (view outputs <$> machines')

-- to fit a 100x100, we need >100 wide and also the correct height

day19_2 :: IO ()
day19_2 = do
  program <- readProgram "input/2019/19.txt"
  let inputs = [[x, y] | x <- [0..49], y <- [0..49]]
      machines = M.fromList [(i, Machine 0 i [] program 0) | i <- inputs]
  runMachines <- sequenceA $ runProgram <$> machines
  let coords = view outputs <$> runMachines
  let grid = MX.matrix 50 50 (\(y, x) -> head . unsafeJ $ M.lookup [fromIntegral x-1, fromIntegral y-1] coords)
  print $ MX.prettyMatrix grid
