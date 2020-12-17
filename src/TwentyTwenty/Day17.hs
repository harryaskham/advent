{-# LANGUAGE QuasiQuotes #-}

module TwentyTwenty.Day17 where

import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Text.RawString.QQ (r)
import Util ((<$$>))

data Cell = Active | Inactive deriving (Eq)

fromChar :: Char -> Cell
fromChar '.' = Inactive
fromChar '#' = Active

type Coord = (Int, Int, Int, Int)

type Bound = (Int, Int)

type Bounds = (Bound, Bound, Bound, Bound)

data Grid = Grid
  { unGrid :: M.Map Coord Cell,
    unBounds :: Bounds,
    unDim :: Dim
  }

data Dim = Dim3 | Dim4

input :: [[Cell]]
input =
  fromChar
    <$$> lines
      [r|...###.#
#.#.##..
.##.##..
..##...#
.###.##.
.#..##..
.....###
.####..#|]

mkGrid :: [[Cell]] -> Dim -> Grid
mkGrid ls dim = Grid grid bounds dim
  where
    grid =
      M.fromList
        [ ((x, y, 0, 0), c)
          | (y, row) <- zip [0 ..] ls,
            (x, c) <- zip [0 ..] row
        ]
    h = length ls
    w = length (head ls)
    bounds =
      ( (-1, w),
        (-1, h),
        (-1, 1),
        case dim of
          Dim3 -> (0, 0)
          Dim4 -> (-1, 1)
      )

growBounds :: Grid -> Bounds
growBounds
  (Grid _ ((x1, x2), (y1, y2), (z1, z2), (w1, w2)) dim) =
    ( (x1 - 1, x2 + 1),
      (y1 - 1, y2 + 1),
      (z1 - 1, z2 + 1),
      case dim of
        Dim3 -> (w1, w2)
        Dim4 -> (w1 - 1, w2 + 1)
    )

getCoords :: Grid -> [Coord]
getCoords (Grid _ ((x1, x2), (y1, y2), (z1, z2), (w1, w2)) dim) =
  [ (x, y, z, w)
    | x <- [x1 .. x2],
      y <- [y1 .. y2],
      z <- [z1 .. z2],
      w <- case dim of
        Dim3 -> [0]
        Dim4 -> [w1 .. w2]
  ]

neighbours :: Grid -> Coord -> [Cell]
neighbours (Grid grid _ dim) (x, y, z, w) =
  catMaybes $ M.lookup <$> neighbourCoords <*> pure grid
  where
    neighbourCoords =
      [ (x + xi, y + yi, z + zi, w + wi)
        | xi <- [-1 .. 1],
          yi <- [-1 .. 1],
          zi <- [-1 .. 1],
          wi <- case dim of
            Dim3 -> [0]
            Dim4 -> [-1 .. 1],
          xi /= 0 || yi /= 0 || zi /= 0 || wi /= 0
      ]

stepCoord :: Grid -> Coord -> Cell
stepCoord g@(Grid grid _ _) coord =
  case M.lookup coord grid of
    Just Active ->
      if activeNs == 2 || activeNs == 3
        then Active
        else Inactive
    _ ->
      if activeNs == 3
        then Active
        else Inactive
  where
    activeNs = length . filter (== Active) $ neighbours g coord

step :: Grid -> Grid
step g@(Grid _ _ dim) = Grid grid' bounds' dim
  where
    grid' =
      M.fromList
        [ (coord, stepCoord g coord)
          | coord <- getCoords g
        ]
    bounds' = growBounds g

stepN :: Int -> Grid -> Grid
stepN 0 grid = grid
stepN n grid = stepN (n -1) (step grid)

solveForDim :: Dim -> Int
solveForDim dim =
  length
    . filter (== Active)
    . fmap snd
    $ M.toList (unGrid $ stepN 6 (mkGrid input dim))

part1 :: Int
part1 = solveForDim Dim3

part2 :: Int
part2 = solveForDim Dim4
