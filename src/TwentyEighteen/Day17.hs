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
import Data.Ord
import qualified Data.PQueue.Prio.Min as PQ
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
        --traceStrLn ("grid:\n" ++ prettyWithWater (M.insert (x, y) Water grid) (S.insert (x, y) nextAllSeen)) $
        (Just allPlacedWater, S.insert (x, y) nextAllSeen)
      | otherwise = go (queue SQ.>< SQ.fromList nextStates) (S.union nextAllSeen seen)
      where
        downState = if not ((x, y + 1) `S.member` seen) && (not ((x, y + 1) `M.member` grid) || M.lookup (x, y + 1) grid == Just Sand) then Just ((x, y + 1), (S.insert (x, y) seen)) else Nothing
        leftState = if not ((x -1, y) `S.member` seen) && M.lookup (x -1, y) grid == Just Sand then Just ((x -1, y), (S.insert (x, y) seen)) else Nothing
        rightState = if not ((x + 1, y) `S.member` seen) && M.lookup (x + 1, y) grid == Just Sand then Just ((x + 1, y), (S.insert (x, y) seen)) else Nothing
        nextStates = case downState of
          Just ds -> [ds]
          Nothing -> catMaybes [leftState, rightState]
        nextAllSeen = foldl' (flip S.insert) allSeen (fst <$> nextStates)
        placedWater = M.insert (x, y) Water grid
        closedSeen = filter (closedIn grid) (S.toList seen)
        allPlacedWater = foldl' (\m p -> M.insert p Water m) placedWater closedSeen

-- always fill from the lowest unseen with no water?

fill :: Int -> Grid Cell -> Coord2 -> (Grid Cell, Set Coord2)
fill maxY grid startPos = go grid (SQ.singleton startPos) S.empty
  where
    go grid SQ.Empty seen = (grid, seen)
    go grid (source SQ.:<| queue) seen =
      traceShow (S.size seen) $
        case gridM of
          Nothing -> (grid, nextSeen)
          Just grid' ->
            let lowestWetClays =
                  SQ.fromList
                    . head
                    . groupOn (snd)
                    . sortOn (Down . snd)
                    . S.toList
                    . S.filter ((/= Water) . (grid' M.!))
                    $ nextSeen
             in --traceStrLn (prettyWithWater grid' nextSeen) $
                go grid' (queue SQ.>< lowestWetClays) nextSeen
      where
        (gridM, seen') = drip maxY grid source
        nextSeen = S.union seen seen'

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
  pretty $ M.filterWithKey (\(_, y) _ -> y < 40) (foldl' (\g p -> if g M.! p /= Water then M.insert p SeenWater g else g) grid (S.toList seen'))
  where
    (minX, minY) = minXY grid
    (maxX, maxY) = maxXY grid
    seen' = S.filter (\(x, y) -> x >= minX && x <= maxX && y >= minY && y <= maxY) seen

-- TODO: abstract into concepts of bucket, and which side it will spill over, and distance to next bucket.
-- spill on one side == distance to bottom, on both it sthat times two
-- buckets in buckets present a problem or maybe they dont? we can fill all at aonce anyway

part1 :: IO Int
part1 = do
  (grid, startPos) <- readGrid
  let (minX, minY) = minXY grid
      (maxX, maxY) = maxXY grid
      (grid', seen) = fill maxY grid startPos
      seen' = S.filter (\(x, y) -> x >= minX && x <= maxX && y >= minY && y <= maxY) seen
  putStrLn $ prettyWithWater grid' seen
  return $ S.size seen'
