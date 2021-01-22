{-# LANGUAGE TupleSections #-}

module TwentyEighteen.Day17 where

import Control.Applicative hiding (many)
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
import Text.ParserCombinators.Parsec hiding ((<|>))
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

data Cell = Sand | Clay | Water | SeenWater | Source deriving (Eq)

instance Show Cell where
  show Sand = " "
  show Clay = "#"
  show Water = "~"
  show SeenWater = "|"
  show Source = "X"

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


mergeCell :: Cell -> Cell -> Cell
mergeCell Sand Water = Water
mergeCell Water Sand = Water
mergeCell a _ = a

dripBfs :: Int -> Grid Cell -> Coord2 -> (Maybe (Grid Cell), Set Coord2, Set Coord2)
dripBfs maxY grid source@(x', y') = massage (go (SQ.singleton ((x', y'), S.empty)) S.empty S.empty S.empty)
  where
    massage (water, seen, sources) =
      if S.null water
        then (Nothing, seen, sources)
        else (Just $ S.foldl' (\m p -> M.insert p Water m) grid water, seen, sources)
    go :: Seq (Coord2, Set Coord2) -> Set Coord2 -> Set Coord2 -> Set Coord2 -> (Set Coord2, Set Coord2, Set Coord2)
    go SQ.Empty waterPositions allSeen sources = (waterPositions, allSeen, sources)
    go ((pos@(x, y), seen) SQ.:<| queue) waterPositions allSeen sources
      | not (pos `M.member` grid) = continue
      | grid M.! pos == Water = continue -- needed for when we get trapped and become the source
      | pos `S.member` seen = continue
      -- | null nextStates && closedIn grid (x, y) =  (Just (M.insert (x, y) Water grid), nextSeen, S.insert (x, y) waterPositions, Just (x, y - 1))
      | null nextStates && closedIn grid (x, y) = go queue (S.insert (x, y) waterPositions) nextAllSeen sources
      | null nextStates = go queue waterPositions nextAllSeen (if snd pos >= maxY then sources else S.insert pos sources) -- if we hit a dead end, we can use it as a new source
      -- | null nextStates = (Nothing, nextSeen, waterPositions, Nothing) -- if we hit a dead end, we can use it as a new source
      | otherwise =
        -- traceShow (pos, S.size seen) $
          --traceStrLnWhen (True) (prettyWithWater grid (seen) source) $
          --mergeBranches (go <$> nextStates <*> pure nextSeen <*> pure waterPositions)
          go nextQueue waterPositions nextAllSeen sources
      where
        continue = go nextQueue waterPositions nextAllSeen sources
        nextAllSeen = S.insert pos allSeen
        nextSeen = S.insert pos seen
        mkState pos@(x, y) =
          if (not (pos `S.member` seen)) && M.lookup pos grid == Just Sand
            then Just pos
            else Nothing
        downState = mkState (x, y + 1)
        leftState = if y < maxY then mkState (x - 1, y) else Nothing
        rightState = if y < maxY then mkState (x + 1, y) else Nothing
        nextPositions = case downState of
          Just ds -> [ds]
          Nothing -> catMaybes [leftState, rightState]
        nextStates = (,nextSeen) <$> nextPositions
        nextQueue = queue SQ.>< SQ.fromList nextStates

-- Maybe just make drip handle a single drip efficiently
dripDfs :: Int -> Grid Cell -> Coord2 -> (Maybe (Grid Cell), Set Coord2, Maybe Coord2)
dripDfs maxY grid source@(x', y') = massage (go (x', y') S.empty S.empty)
  where
    massage (water, seen, sources) =
      if S.null water
        then (Nothing, seen, sources)
        else (Just $ S.foldl' (\m p -> M.insert p Water m) grid water, seen, sources)
    go :: Coord2 -> Set Coord2 -> Set Coord2 -> (Set Coord2, Set Coord2, Maybe Coord2)
    go pos@(x, y) seen waterPositions
      | not (pos `M.member` grid) = (waterPositions, seen, Nothing)
      | grid M.! pos == Water = (waterPositions, seen, Nothing) -- needed for when we get trapped and become the source
      | pos `S.member` seen =  (waterPositions, seen, Nothing)
      -- | null nextStates && closedIn grid (x, y) =  (Just (M.insert (x, y) Water grid), nextSeen, S.insert (x, y) waterPositions, Just (x, y - 1))
      | null nextStates && closedIn grid (x, y) = (S.insert (x, y) waterPositions, nextSeen, Nothing)
      | null nextStates = (waterPositions, nextSeen, if snd pos >= maxY then Nothing else Just pos) -- if we hit a dead end, we can use it as a new source
      -- | null nextStates = (Nothing, nextSeen, waterPositions, Nothing) -- if we hit a dead end, we can use it as a new source
      | otherwise =
        -- traceShow (pos, S.size seen) $
          --traceStrLnWhen (True) (prettyWithWater grid (seen) source) $
          mergeBranches (go <$> nextStates <*> pure nextSeen <*> pure waterPositions)
      where
        mergeBranches bs = (mergedSeen, mergedWaterPositions, mergedSources)
          where
            mergedSeen = foldl1 S.union (fst3 <$> bs)
            mergedWaterPositions = foldl1 S.union (snd3 <$> bs)
            mergedSources = case catMaybes (thd3 <$> bs) of
              [] -> Nothing
              sources -> Just $ minimumOn snd sources -- if we filled 1+ reservoirs with this drip, take the highest as our new source
        nextSeen = S.insert (x, y) seen
        mkState pos@(x, y) =
          if (not (pos `S.member` seen)) && M.lookup pos grid == Just Sand
            then Just pos
            else Nothing
        downState = mkState (x, y + 1)
        leftState = if y < maxY then mkState (x - 1, y) else Nothing
        rightState = if y < maxY then mkState (x + 1, y) else Nothing
        nextStates = case downState of
          Just ds -> [ds]
          Nothing -> catMaybes [leftState, rightState]


fillDfs :: Int -> Grid Cell -> Coord2 -> (Grid Cell, Set Coord2)
fillDfs maxY grid startPos = go grid S.empty (S.singleton (swap startPos))
  where
    go grid seen sources
      -- | (maxY + 1) `S.member` S.map snd seen' = (grid, S.map swap nextSeen) -- stop if we overflow the bottom
      -- | source == startPos && isNothing gridM = (grid, S.map swap nextSeen) -- stop if we cant drip from top ever
      | S.null sources = (grid, S.map swap nextSeen) -- stop if we cant drip from top ever
      | otherwise =
        traceShow (S.size nextSeen, source, S.size sources) $
          traceStrLnWhen (S.size nextSeen `mod` 7000 == 0) (prettyWithWater grid (S.map swap seen) source) $
          --traceStrLnWhen (S.size waterPositions > 24400) (prettyWithWater grid (S.map swap seen) source) $
          --traceStrLnWhen (True) (prettyWithWater grid (S.map swap seen) source) $
          -- traceStrLnWhen (S.size waterPositions == 368) (prettyWithWater grid (S.map swap seen) source) $
          case gridM of
            -- Nothing -> goFromNewSourceAbove
            Nothing -> go grid nextSeen nextSources -- if we can't drip from here, move up one
            Just grid' -> go grid' nextSeen nextSources
      where
        (source, sources') = case S.maxView sources of
          Just (s, ss) -> (swap s, ss)
          Nothing -> (startPos, S.empty)
        (gridM, seen', nextSourceM) = dripDfs maxY grid source
        -- If we failed to drop, search for a new pos. Otherwise move to the last point of overflow
        nextSources =
          case gridM of
            Nothing -> sources'
            Just _ -> case nextSourceM of
              Nothing -> sources
              Just s -> S.insert (swap s) sources
        nextSeen = seen `S.union` S.map swap seen'

fillBfs :: Int -> Grid Cell -> Coord2 -> (Grid Cell, Set Coord2)
fillBfs maxY grid startPos = go grid S.empty (S.singleton (swap startPos))
  where
    go grid seen sources
      -- | (maxY + 1) `S.member` S.map snd seen' = (grid, S.map swap nextSeen) -- stop if we overflow the bottom
      -- | source == startPos && isNothing gridM = (grid, S.map swap nextSeen) -- stop if we cant drip from top ever
      | S.null sources = (grid, nextSeen) -- stop if we cant drip from top ever
      | otherwise =
        traceShow (S.size nextSeen, source, S.size sources) $
          --traceStrLnWhen (S.size nextSeen `mod` 1000 == 0) (prettyWithWater grid seen source) $
          traceStrLnWhen (S.size nextSeen == 29427) (prettyWithWater grid seen source) $
          case gridM of
            Nothing -> go grid nextSeen nextSources -- if we can't drip from here, move up one
            Just grid' -> go grid' nextSeen nextSources
      where
        (source, sourcesWithout) = case S.maxView sources of
          Just (s, ss) -> (swap s, ss)
          Nothing -> (startPos, S.empty)
        (gridM, newSeen, newSources) = dripBfs maxY grid source
        -- If we failed to drop, search for a new pos. Otherwise move to the last point of overflow
        nextSources = case gridM of
          Nothing -> sourcesWithout `S.union` (S.map swap newSources)
          Just _ -> sources `S.union` (S.map swap newSources)
        nextSeen = seen `S.union` newSeen

readGrid :: IO (Grid Cell, Coord2, Int)
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
              y <- [0 .. maxY + 1]
          ]
  return (grid, startPos, maxY)

prettyWithWater :: Grid Cell -> Set Coord2 -> Coord2 -> String
prettyWithWater grid seen source =
  pretty $ M.insert source Source $ (foldl' (\g p -> if g M.! p /= Water then M.insert p SeenWater g else g) grid (S.toList seen'))
  where
    (minX, minY) = minXY grid
    (maxX, maxY) = maxXY grid
    seen' = S.filter (\(x, y) -> x >= minX && x <= maxX && y >= minY && y <= maxY) seen

part1 :: IO Int
part1 = do
  (grid, startPos, maxY) <- readGrid
  --let (grid', seen) = fillDfs maxY grid startPos
  let (grid', seen) = fillBfs maxY grid startPos
  putStrLn $ prettyWithWater grid' seen startPos
  return $ S.size (S.filter ((<= maxY) . snd) seen)
