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
import qualified Data.PQueue.Prio.Max as PQ
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
  show Sand = " "
  show Clay = "#"
  show Water = "~"
  show SeenWater = "|"

closedIn :: Grid Cell -> Coord2 -> Bool
closedIn grid (x, y) = closedToRight (x, y) && closedToLeft (x, y)
  where
    closedToRight (x, y)
      | M.lookup (x, y + 1) grid `elem` [Nothing, Just Sand] = False
      | M.lookup (x + 1, y) grid == Nothing = False
      | M.lookup (x + 1, y) grid == Just Clay = True
      | otherwise = closedToRight (x + 1, y)
    closedToLeft (x, y)
      | M.lookup (x, y + 1) grid `elem` [Nothing, Just Sand] = False
      | M.lookup (x - 1, y) grid == Nothing = False
      | M.lookup (x - 1, y) grid == Just Clay = True
      | otherwise = closedToLeft (x - 1, y)

drip :: Int -> Grid Cell -> Coord2 -> (Grid Cell, Set Coord2)
drip maxY grid' (x, y) = go grid' (SQ.singleton ((x, y), S.empty)) S.empty
  where
    go grid SQ.Empty allSeen = (grid, S.insert (x, y) allSeen)
    go grid (((x, y), seen) SQ.:<| queue) allSeen
      | y == maxY = go placedWater queue (S.union nextAllSeen seen)
      | (x, y) `S.member` seen = go grid queue allSeen
      | null nextStates && closedIn grid (x, y) =
        --traceStrLn ("grid:\n" ++ prettyWithWater (M.insert (x, y) Water grid) (S.insert (x, y) nextAllSeen)) $
        (placedWater, S.insert (x, y) nextAllSeen)
      | otherwise = go grid (queue SQ.>< SQ.fromList nextStates) (S.union nextAllSeen seen)
      where
        nextSeen = S.insert (x, y) seen
        mkState pos =
          if (not (pos `S.member` (seen `S.union` allSeen))) && M.lookup pos grid == Just Sand
            then Just (pos, nextSeen)
            else Nothing
        downState = mkState (x, y + 1)
        leftState = if (x -1, y + 1) `M.member` grid then mkState (x -1, y) else Nothing
        rightState = if (x + 1, y + 1) `M.member` grid then mkState (x + 1, y) else Nothing
        nextStates = case downState of
          Just ds -> [ds]
          Nothing -> catMaybes [leftState, rightState]
        nextAllSeen = foldl' (flip S.insert) allSeen (fst <$> nextStates)
        closedSeen = filter (closedIn grid) ((x, y) : S.toList seen)
        placedWater = foldl' (\m p -> M.insert p Water m) grid closedSeen

fill :: Int -> Grid Cell -> Coord2 -> (Grid Cell, Set Coord2)
fill maxY grid startPos =
  go grid (PQ.singleton (snd startPos) startPos) S.empty S.empty
  where
    go grid queue seen filledFrom
      | PQ.null queue = (grid, seen)
      | snd source > maxY = go grid rest seen nextFilledFrom
      | source `S.member` filledFrom = go grid rest seen nextFilledFrom
      | M.lookup source grid `elem` [Nothing, Just Water] = go grid rest seen filledFrom
      | otherwise =
        traceShow (S.size seen, source, M.lookup source grid, S.size seen', length wetClay) $
          traceStrLnWhen (S.size seen == 27176) (prettyWithWater grid seen) $
            go grid' (foldl' (\q (x, y) -> PQ.insert y (x, y) q) rest wetClay) nextSeen nextFilledFrom
      where
        ((_, source), rest) = PQ.deleteFindMax queue
        (grid', seen') = drip maxY grid source
        wetClay = SQ.fromList . S.toList . S.filter (not . (`elem` [Nothing, Just Water]) . (`M.lookup` grid')) $ seen'
        nextSeen = S.union seen seen'
        nextFilledFrom = S.insert source filledFrom

readGrid :: IO (Grid Cell, Coord2)
readGrid = do
  veins <- readWithParser veins <$> input 2018 17
  --veins <- readWithParser veins <$> exampleInput 2018 17
  let clayCoords = S.fromList $ concat veins
      minX = fst $ minimumOn fst (concat veins)
      maxX = fst $ maximumOn fst (concat veins)
      maxY = snd $ maximumOn snd (concat veins)
      startPos = (500 - minX + 1, 1)
      grid =
        M.fromList
          [ ((x, y), if (x + minX - 1, y) `S.member` clayCoords then Clay else Sand)
            | x <- [0 .. (maxX - minX) + 2],
              y <- [0 .. maxY]
          ]
  return (grid, startPos)

prettyWithWater :: Grid Cell -> Set Coord2 -> String
prettyWithWater grid seen =
  pretty $ (foldl' (\g p -> if g M.! p /= Water then M.insert p SeenWater g else g) grid (S.toList seen'))
  where
    (minX, minY) = minXY grid
    (maxX, maxY) = maxXY grid
    seen' = S.filter (\(x, y) -> x >= minX && x <= maxX && y >= minY && y <= maxY) seen

part1 :: IO Int
part1 = do
  (grid, startPos) <- readGrid
  let (minX, minY) = minXY grid
      (maxX, maxY) = maxXY grid
      (grid', seen) = fill maxY grid startPos
      seen' = S.filter (\(x, y) -> x >= minX && x <= maxX && y >= minY && y <= maxY) seen
  putStrLn $ prettyWithWater grid' seen
  return $ S.size seen'
