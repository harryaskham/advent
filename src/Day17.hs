{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiWayIf #-}

module Day17 where

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
import TwentyNineteen (Machine(..), readProgram, stepUntilNOutputs, inputs, outputs, clear, runProgram)

data Space = Empty | Scaffold | RUp | RDown | RLeft | RRight | RFallen deriving (Eq)

spaceFromChar '.' = Empty
spaceFromChar '#' = Scaffold
spaceFromChar '^' = RUp
spaceFromChar 'v' = RDown
spaceFromChar '>' = RRight
spaceFromChar '<' = RLeft
spaceFromChar 'X' = RFallen
spaceFromChar e = error $ show e

instance Show Space where
  show Empty = "."
  show Scaffold = "#"
  show RUp = "^"
  show RDown = "v"
  show RRight = ">"
  show RLeft = "<"
  show RFallen = "X"

asciiToGrid :: [Integer] -> [[Space]]
asciiToGrid outputs = (fmap . fmap) spaceFromChar (take (length rows - 1) rows)
  where
    ascii = toEnum . fromIntegral <$> outputs
    rows = lines ascii

intersectionCoords :: [[Space]] -> [(Int, Int)]
intersectionCoords grid = filter isIntersection coords
  where
    coords = [(x, y) | x <- [1 .. length (head grid) - 2], y <- [1 .. length grid - 2]]
    isIntersection (x, y) = ((grid !! y !! x) == Scaffold)
                            && ((grid !! (y+1) !! x) == Scaffold)
                            && ((grid !! (y-1) !! x) == Scaffold)
                            && ((grid !! y !! (x+1)) == Scaffold)
                            && ((grid !! y !! (x-1)) == Scaffold)
    
day17_1 :: IO ()
day17_1 = do
  program <- readProgram "input/2019/17.txt"
  machine <- runProgram (Machine 0 [] [] program 0)
  let grid = asciiToGrid $ machine ^. outputs
      intersections = intersectionCoords grid
      alignmentParams = ((*) <$> fst <*> snd) <$> intersections
  sequenceA_ $ print <$> grid
  print $ sum alignmentParams

inputMovements :: String -> String -> String -> String -> Bool -> Machine -> Machine
inputMovements routine funcA funcB funcC cameraOn machine =
  machine & inputs .~ concat [asciiRoutine, asciiFuncA, asciiFuncB, asciiFuncC, asciiCamera]
  where
    asciiRoutine = fromIntegral . fromEnum <$> routine ++ "\n"
    asciiFuncA = fromIntegral . fromEnum <$> funcA ++ "\n"
    asciiFuncB = fromIntegral . fromEnum <$> funcB ++ "\n"
    asciiFuncC = fromIntegral . fromEnum <$> funcC ++ "\n"
    asciiCamera = [ if cameraOn
                      then (fromIntegral . fromEnum) 'y'
                      else (fromIntegral . fromEnum) 'n'
                  , (fromIntegral . fromEnum) '\n']

day17_2 :: IO ()
day17_2 = do
  program <- readProgram "input/2019/17.txt"
  let machine = Machine 0 [] [] (M.insert 0 2 program) 0
      moved = inputMovements
                "A,B,C"
                "L,4,L,4,L,10,R,4,R,4"
                "L,4,L,4"
                "R,8,R,10"
                False
                machine
  print $ moved ^. inputs
  runMoved <- runProgram moved
  let rows = splitOn "\n" $ toEnum . fromIntegral <$> runMoved ^. outputs
  sequenceA_ $ print <$> rows
  print $ last $ runMoved ^. outputs
