{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module TwentyNineteen.Day17 where

import Control.Applicative
import Control.Concurrent
import Control.Lens hiding (Empty)
import Control.Monad
import Data.Char
import Data.Foldable
import Data.Function ((&))
import Data.List
import qualified Data.List.Safe as LS
import Data.List.Split hiding (condense)
import qualified Data.Map.Strict as M
import qualified Data.Matrix as MX
import Data.Maybe
import Data.Ord
import Data.Ratio
import qualified Data.Set as S
import qualified Data.Tree as T
import qualified Data.Vector as V
import qualified Data.Vector.Split as VS
import Debug.Trace
import System.IO
import System.IO.HiddenChar
import System.Random
import Text.ParserCombinators.ReadP
import TwentyNineteen.Intcode

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
    isIntersection (x, y) =
      ((grid !! y !! x) == Scaffold)
        && ((grid !! (y + 1) !! x) == Scaffold)
        && ((grid !! (y -1) !! x) == Scaffold)
        && ((grid !! y !! (x + 1)) == Scaffold)
        && ((grid !! y !! (x -1)) == Scaffold)

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
    asciiCamera =
      [ if cameraOn
          then (fromIntegral . fromEnum) 'y'
          else (fromIntegral . fromEnum) 'n',
        (fromIntegral . fromEnum) '\n'
      ]

-- Full string:
-- A L,4,L,4,L,10,R,4,
-- B R,4,L,4,L,4,R,8,R,10,
-- A L,4,L,4,L,10,R,4,
-- C R,4,L,10,R,10,
-- A L,4,L,4,L,10,R,4,
-- C R,4,L,10,R,10,
-- B R,4,L,4,L,4,R,8,R,10,
-- C R,4,L,10,R,10,
-- C R,4,L,10,R,10,
-- B R,4,L,4,L,4,R,8,R,10

day17_2 :: IO ()
day17_2 = do
  program <- readProgram "input/2019/17.txt"
  let machine = Machine 0 [] [] (M.insert 0 2 program) 0
      moved =
        inputMovements
          "A,B,A,C,A,C,B,C,C,B"
          "L,4,L,4,L,10,R,4"
          "R,4,L,4,L,4,R,8,R,10"
          "R,4,L,10,R,10"
          False
          machine
  runMoved <- runProgram moved
  let rows = splitOn "\n" $ toEnum . fromIntegral <$> runMoved ^. outputs
  sequenceA_ $ print <$> rows
  print $ last $ runMoved ^. outputs
