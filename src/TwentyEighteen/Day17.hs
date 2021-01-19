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

data Cell = Sand | Clay | Water | SeenWater deriving (Eq)

instance Show Cell where
  show Sand = "."
  show Clay = "#"
  show Water = "~"
  show SeenWater = "|"

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

drip :: Int -> Grid Cell -> Coord2 -> (Maybe (Grid Cell), Set Coord2)
drip maxY grid (x, y) = go (SQ.singleton ((x, y), S.empty)) S.empty
  where
    go SQ.Empty allSeen = (Nothing, S.insert (x, y) allSeen)
    go (((x, y), seen) SQ.:<| queue) allSeen
      | y > maxY = go queue (S.union nextAllSeen seen)
      | null nextStates && closedIn grid (x, y) =
        traceStrLn ("grid:\n" ++ prettyWithWater (M.insert (x, y) Water grid) (S.insert (x, y) nextAllSeen)) $
          --traceShow "landed water" $
          (Just (M.insert (x, y) Water grid), S.insert (x, y) nextAllSeen)
      | otherwise = go (queue SQ.>< SQ.fromList nextStates) (S.union nextAllSeen seen)
      where
        downState = if not ((x, y + 1) `S.member` seen) && (not ((x, y + 1) `M.member` grid) || M.lookup (x, y + 1) grid == Just Sand) then Just ((x, y + 1), (S.insert (x, y) seen)) else Nothing
        leftState = if not ((x -1, y) `S.member` seen) && M.lookup (x -1, y) grid == Just Sand then Just ((x -1, y), (S.insert (x, y) seen)) else Nothing
        rightState = if not ((x + 1, y) `S.member` seen) && M.lookup (x + 1, y) grid == Just Sand then Just ((x + 1, y), (S.insert (x, y) seen)) else Nothing
        nextStates = case downState of
          Just ds -> [ds]
          Nothing -> catMaybes [leftState, rightState]
        nextAllSeen = foldl' (flip S.insert) allSeen (fst <$> nextStates)

fill :: Grid Cell -> Coord2 -> Set Coord2 -> (Grid Cell, Set Coord2)
fill grid startPos seen =
  let (_, maxY) = maxXY grid
      (gridM, seen') = drip maxY grid startPos
   in case gridM of
        Nothing -> (grid, S.union seen seen')
        Just grid' ->
          fill grid' startPos (S.union seen seen')

readGrid :: IO (Grid Cell, Coord2)
readGrid = do
  veins <- readWithParser veins <$> input 2018 17
  let clayCoords = S.fromList $ concat veins
      minX = fst $ minimumOn fst (concat veins)
      maxX = fst $ maximumOn fst (concat veins)
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

prettyWithWater :: Grid Cell -> Set Coord2 -> String
prettyWithWater grid seen =
  (pretty (foldl' (\g p -> if g M.! p /= Water then M.insert p SeenWater g else g) grid (S.toList seen')))
  where
    (minX, minY) = minXY grid
    (maxX, maxY) = maxXY grid
    seen' = S.filter (\(x, y) -> x >= minX && x <= maxX && y >= minY && y <= maxY) seen

part1 :: IO Int
part1 = do
  (grid, startPos) <- readGrid
  let (grid', seen) = fill grid startPos S.empty
      (minX, minY) = minXY grid
      (maxX, maxY) = maxXY grid
      seen' = S.filter (\(x, y) -> x >= minX && x <= maxX && y >= minY && y <= maxY) seen
  putStrLn $ prettyWithWater grid' seen
  return $ S.size seen'
