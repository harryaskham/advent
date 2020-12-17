{-# LANGUAGE QuasiQuotes #-}

module TwentyTwenty.Day17 where

import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Text.RawString.QQ (r)
import Util ((<$$>))

data Cell = Active | Inactive deriving (Eq)

fromChar :: Char -> Cell
fromChar '.' = Inactive
fromChar '#' = Active

type Coord = (Int, Int, Int)

type Bound = (Int, Int)

type Bounds = (Bound, Bound, Bound)

data Grid = Grid
  { unGrid :: M.Map Coord Cell,
    unBounds :: Bounds
  }

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

dI :: [[Cell]]
dI =
  fromChar
    <$$> lines
      [r|.#.
..#
###|]

mkGrid :: [[Cell]] -> Grid
mkGrid ls = Grid grid bounds
  where
    grid = M.fromList [((x, y, 0), c) | (y, row) <- zip [0 ..] ls, (x, c) <- zip [0 ..] row]
    h = length ls
    w = length (head ls)
    bounds = ((-1, w), (-1, h), (-1, 1))

growBounds :: Bounds -> Bounds
growBounds ((x1, x2), (y1, y2), (z1, z2)) =
  ((x1 - 1, x2 + 1), (y1 - 1, y2 + 1), (z1 - 1, z2 + 1))

boundsToCoords :: Bounds -> [Coord]
boundsToCoords ((x1, x2), (y1, y2), (z1, z2)) =
  [(x, y, z) | x <- [x1 .. x2], y <- [y1 .. y2], z <- [z1 .. z2]]

neighbours :: Grid -> Coord -> [Cell]
neighbours (Grid grid _) (x, y, z) =
  catMaybes $ M.lookup <$> neighbourCoords <*> pure grid
  where
    neighbourCoords =
      [ (x + xi, y + yi, z + zi)
        | xi <- [-1 .. 1],
          yi <- [-1 .. 1],
          zi <- [-1 .. 1],
          (xi /= 0 || yi /= 0 || zi /= 0)
      ]

stepCoord :: Grid -> Coord -> Cell
stepCoord g@(Grid grid _) coord =
  case M.lookup coord grid of
    Just Active -> if activeNs == 2 || activeNs == 3 then Active else Inactive
    _ -> if activeNs == 3 then Active else Inactive
  where
    activeNs = length [n | n <- neighbours g coord, n == Active]

step :: Grid -> Grid
step g@(Grid grid bounds) = Grid grid' bounds'
  where
    grid' = M.fromList [(coord, stepCoord g coord) | coord <- boundsToCoords bounds]
    bounds' = growBounds bounds

stepN :: Int -> Grid -> Grid
stepN 0 grid = grid
stepN n grid = stepN (n -1) (step grid)

part1 :: Int
part1 = length [c | c <- snd <$> M.toList (unGrid $ stepN 6 (mkGrid input)), c == Active]

toChar Active = '#'
toChar Inactive = '.'

fromJ (Just c) = c
fromJ Nothing = Inactive

pretty (Grid grid ((x1, x2), (y1, y2), (z1, z2))) =
  [intercalate "\n" $ [[toChar (fromJ (M.lookup (x, y, z) grid)) | x <- [x1 .. x2]] | y <- [y1 .. y2]] | z <- [z1 .. z2]]

stepNIO :: Int -> Grid -> IO Grid
stepNIO 0 grid = return grid
stepNIO n grid = do
  traverse putStrLn (pretty grid)
  stepNIO (n -1) (step grid)

debug :: IO ()
debug = do
  let grid = mkGrid dI
  stepNIO 6 grid
  return ()
