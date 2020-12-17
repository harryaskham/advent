{-# LANGUAGE QuasiQuotes #-}

module TwentyTwenty.Day17 where

import Data.List (nub)
import qualified Data.Set as S
import Text.RawString.QQ (r)

type Coord = (Int, Int, Int, Int)

data Grid = Grid
  { unGrid :: S.Set Coord,
    unDim :: Dim
  }

data Dim = Dim3 | Dim4

input :: [String]
input =
  lines
    [r|...###.#
#.#.##..
.##.##..
..##...#
.###.##.
.#..##..
.....###
.####..#|]

mkGrid :: [String] -> Dim -> Grid
mkGrid ls = Grid grid
  where
    grid =
      S.fromList
        [ (x, y, 0, 0)
          | (y, row) <- zip [0 ..] ls,
            (x, c) <- zip [0 ..] row,
            c == '#'
        ]

getCoords :: Grid -> [Coord]
getCoords (Grid grid dim) = nub $ concatMap (neighbours dim) (S.toList grid)

neighbours :: Dim -> Coord -> [Coord]
neighbours dim (x, y, z, w) =
  [ (x + xi, y + yi, z + zi, w + wi)
    | xi <- [-1 .. 1],
      yi <- [-1 .. 1],
      zi <- [-1 .. 1],
      wi <- case dim of
        Dim3 -> [0]
        Dim4 -> [-1 .. 1],
      xi /= 0 || yi /= 0 || zi /= 0 || wi /= 0
  ]

shouldSetActive :: Grid -> Coord -> Bool
shouldSetActive (Grid grid dim) coord =
  if coord `S.member` grid
    then activeNs == 2 || activeNs == 3
    else activeNs == 3
  where
    activeNs = length . filter (`S.member` grid) $ neighbours dim coord

step :: Grid -> Grid
step g@(Grid _ dim) = Grid grid' dim
  where
    grid' = S.fromList [coord | coord <- getCoords g, shouldSetActive g coord]

stepN :: Int -> Grid -> Grid
stepN 0 grid = grid
stepN n grid = stepN (n - 1) (step grid)

solveForDim :: Dim -> Int
solveForDim =
  S.size
    . unGrid
    . stepN 6
    . mkGrid input

part1 :: Int
part1 = solveForDim Dim3

part2 :: Int
part2 = solveForDim Dim4
