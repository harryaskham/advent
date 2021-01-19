module TwentyEighteen.Day17 where

import Coord
import Data.Bits
import Data.Char
import qualified Data.Foldable as F
import Data.Function
import Data.List
import Data.List.Extra
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Sequence (Seq)
import qualified Data.Sequence as SQ
import Data.Set (Set)
import qualified Data.Set as S
import Data.Tuple.Extra
import Data.Vector (Vector)
import qualified Data.Vector as V
import Debug.Trace
import Grid
import Text.ParserCombinators.Parsec
import Util

type Vein = [Coord2]

veins :: GenParser Char () [Vein]
veins = do
  vs <- many vein
  eof
  return vs
  where
    vein = do
      dim1 <- xy
      char '='
      v <- read <$> many1 digit
      string ", "
      xy
      char '='
      low <- read <$> many1 digit
      string ".."
      high <- read <$> many1 digit
      eol
      return $ case dim1 of
        'x' -> [(v, y) | y <- [low .. high]]
        'y' -> [(x, v) | x <- [low .. high]]
    xy = oneOf "xy"

data Cell = Sand | Clay | Water deriving (Eq)

instance Show Cell where
  show Sand = "."
  show Clay = "#"
  show Water = "~"

readGrid :: IO (Grid Cell, Coord2)
readGrid = do
  veins <- readWithParser veins <$> exampleInput 2018 17
  let clayCoords = S.fromList $ concat veins
      minX = fst $ minimumOn fst (concat veins)
      maxX = fst $ maximumOn fst (concat veins)
      minY = snd $ maximumOn snd (concat veins)
      maxY = snd $ maximumOn snd (concat veins)
      startPos = (500 - minX + 1, 1)
      grid =
        M.insert startPos Water $
          M.fromList
            [ ((x, y), if (x + minX - 1, y) `S.member` clayCoords then Clay else Sand)
              | x <- [0 .. (maxX - minX) + 2],
                y <- [0 .. maxY]
            ]
  return (grid, startPos)

closedIn :: Grid Cell -> Coord2 -> Bool
closedIn grid (x, y) = closedToRight (x, y) && closedToLeft (x, y)
  where
    closedToRight (x, y)
      | M.lookup (x, y + 1) grid == Just Sand = False
      | M.lookup (x + 1, y) grid == Nothing = False
      | M.lookup (x + 1, y) grid == Just Clay = True
      | otherwise = closedToRight (x + 1, y)
    closedToLeft (x, y)
      | M.lookup (x, y + 1) grid == Just Sand = False
      | M.lookup (x - 1, y) grid == Nothing = False
      | M.lookup (x - 1, y) grid == Just Clay = True
      | otherwise = closedToLeft (x - 1, y)

-- TODO: Need to be able to backtrack here i.e. search for the droplet location
drip :: Grid Cell -> Coord2 -> (Grid Cell, Set Coord2)
drip grid (x, y) = go (x, y) S.empty
  where
    go :: Coord2 -> Set Coord2 -> (Grid Cell, Set Coord2)
    go (x, y) seen
      | y >= maxY = (grid, S.insert (x, y) seen)
      | not ((x, y + 1) `S.member` seen) && M.lookup (x, y + 1) grid == Just Sand = go (x, y + 1) (S.insert (x, y) seen)
      | not ((x - 1, y) `S.member` seen) && M.lookup (x - 1, y) grid == Just Sand = go (x - 1, y) (S.insert (x, y) seen)
      | not ((x + 1, y) `S.member` seen) && M.lookup (x + 1, y) grid == Just Sand = go (x + 1, y) (S.insert (x, y) seen)
      | closedIn grid (x, y) = (M.insert (x, y) Water grid, S.insert (x, y) seen)
      | otherwise = (grid, S.insert (x, y) seen)
      where
        (_, maxY) = maxXY grid

fill :: Grid Cell -> Coord2 -> Int -> Set Coord2 -> (Grid Cell, Set Coord2)
fill grid _ 0 seen = (grid, seen)
fill grid startPos n seen =
  let (grid', seen') = drip grid startPos
   in traceStrLn (pretty grid) $ fill grid' startPos (n -1) (S.union seen seen')

part1 :: IO Int
part1 = do
  (grid, startPos) <- readGrid
  return $ S.size $ snd $ fill grid startPos 500 S.empty
