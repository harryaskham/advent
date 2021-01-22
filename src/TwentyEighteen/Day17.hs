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

{-
dripBfs :: Int -> Grid Cell -> Coord2 -> (Grid Cell, Set Coord2)
dripBfs maxY grid' (x, y) = go grid' (SQ.singleton ((x, y), S.empty)) S.empty
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
-}

mergeCell :: Cell -> Cell -> Cell
mergeCell Sand Water = Water
mergeCell Water Sand = Water
mergeCell a _ = a

-- Maybe just make drip handle a single drip efficiently
dripDfs :: Int -> Grid Cell -> Coord2 -> Set Coord2 -> Set (Coord2, Set Coord2) -> (Maybe (Grid Cell), Set Coord2, Set Coord2, Maybe Coord2)
dripDfs maxY grid' source@(x', y') waterPositions' seenSourceWater = go grid' (x', y') S.empty waterPositions'
  where
    go :: Grid Cell -> Coord2 -> Set Coord2 -> Set Coord2 -> (Maybe (Grid Cell), Set Coord2, Set Coord2, Maybe Coord2)
    go grid pos@(x, y) seen waterPositions
      -- | pos /= source && (pos, waterPositions) `S.member` seenSourceWater = traceShow "cached pos" $ (Nothing, seen, waterPositions)
      | not (pos `M.member` grid) = (Nothing, seen, waterPositions, Nothing)
      | grid M.! pos == Water =  (Nothing,  seen, waterPositions, Nothing) -- needed for when we get trapped and become the source
      | pos `S.member` seen =  (Nothing, seen, waterPositions, Nothing)
      | null nextStates && closedIn grid (x, y) =  (Just (M.insert (x, y) Water grid), nextSeen, S.insert (x, y) waterPositions, Nothing)
      | null nextStates =  (Nothing, nextSeen, waterPositions, Just pos) -- if we hit a dead end, we can use it as a new source
      | otherwise =
        -- traceShow (pos, S.size seen) $
          --traceStrLnWhen (True) (prettyWithWater grid (seen) source) $
          mergeBranches (go grid <$> nextStates <*> pure nextSeen <*> pure waterPositions)
      where
        closedSeen = filter (closedIn grid) ((x, y) : S.toList seen)
        -- TODO: If the map merging is too slow, we can IORef the grid
        mergeBranches :: [(Maybe (Grid Cell), Set Coord2, Set Coord2, Maybe Coord2)] -> (Maybe (Grid Cell), Set Coord2, Set Coord2, Maybe Coord2)
        --mergeBranches bs = (withClosedSeen, mergedSeen, mergedWaterPositions)
        mergeBranches bs = (mergedGrids, mergedSeen, mergedWaterPositions, mergedSources)
          where
            branchesWithDrips = catMaybes (fst4 <$> bs)
            mergedGrids =
              case branchesWithDrips of
                [] -> Nothing
                -- bs -> Just $ foldl1 (M.unionWith mergeCell) bs -- just merge all the maps
                -- bs -> Just $ head bs -- temporarily avoid merging by throwing away other good drips
                _ -> Just $ foldl' (\m p -> M.insert p Water m) grid (S.toList $ mergedWaterPositions `S.difference` waterPositions) -- just fold in any new water position
            withClosedSeen = (foldl' (\m p -> M.insert p Water m)) <$> (mergedGrids <|> (Just grid)) <*> (pure closedSeen)
            mergedSeen = foldl1 S.union (snd4 <$> bs)
            mergedWaterPositions = foldl1 S.union (thd4 <$> bs)
            mergedSources = case catMaybes (fth4 <$> bs) of
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

{-
fillBfs :: Int -> Grid Cell -> Coord2 -> (Grid Cell, Set Coord2)
fillBfs maxY grid startPos =
  go grid (PQ.singleton (snd startPos) startPos) S.empty S.empty
  where
    go grid queue seen filledFrom
      | PQ.null queue = (grid, seen)
      | snd source > maxY = go grid rest seen nextFilledFrom
      | source `S.member` filledFrom = go grid rest seen nextFilledFrom
      | M.lookup source grid `elem` [Nothing, Just Water] = go grid rest seen filledFrom
      | otherwise =
        traceShow (S.size seen, source, M.lookup source grid, S.size seen', length wetClay) $
          traceStrLnWhen (True) (prettyWithWater grid seen) $
            go grid' (foldl' (\q (x, y) -> PQ.insert y (x, y) q) rest wetClay) nextSeen nextFilledFrom
      where
        ((_, source), rest) = PQ.deleteFindMax queue
        (grid', seen') = drip maxY grid source
        wetClay = SQ.fromList . S.toList . S.filter (not . (`elem` [Nothing, Just Water]) . (`M.lookup` grid')) $ seen'
        nextSeen = S.union seen seen'
        nextFilledFrom = S.insert source filledFrom
-}

getSource :: Grid Cell -> Set Coord2 -> Coord2 -> Maybe Coord2 -> Coord2
getSource grid seen def lastSourceM
  | S.null ps = def
  | otherwise =
    case lastSourceM of
      -- If we provide a last source then look upward for another
      Just (_, y) -> case S.lookupMax (S.filter (\(y', _) -> y' < y) ps) of
        Just source -> source
        Nothing -> def
      -- Otherwise go lower
      Nothing -> swap $ S.findMax ps
  where
    ps = S.filter (\p -> M.lookup (swap p) grid == Just Sand) seen

getNextSource grid seen lastSource@(lx, ly) def = go seen
  where
    go seen =
      case S.maxView seen of
        Just ((y, x), seen') -> if y < ly then (x, y) else go seen'
        Nothing -> def

-- Track swapped seen so we can findmin on Y in logtime
-- TODO: but then we get stuck pouring from a dead end
-- Track water positions and sources. If we saw that before then go long ya bizniss
fillDfs :: Int -> Grid Cell -> Coord2 -> (Grid Cell, Set Coord2)
fillDfs maxY grid startPos = go grid S.empty startPos S.empty S.empty
  where
    go grid seen source waterPositions seenSourceWater
      -- | (maxY + 1) `S.member` S.map snd seen' = (grid, S.map swap nextSeen) -- stop if we overflow the bottom
      | source == startPos && isNothing gridM = (grid, S.map swap nextSeen) -- stop if we cant drip from top ever
      -- | isNothing gridM = (grid, S.map swap nextSeen)
      -- TODO: bettersource choice below
      | isNothing gridM = go grid seen nextSource nextWaterPositions nextSeenSourceWater -- if we can't drip from here, move up one
      -- | (source, waterPositions) `S.member` seenSourceWater = goFromNewSourceAbove
      | otherwise =
        traceShow (S.size waterPositions, S.size nextSeen, source) $
          traceStrLnWhen (S.size waterPositions `mod` 3000 == 0) (prettyWithWater grid (S.map swap seen) source) $
          -- traceStrLnWhen (S.size waterPositions == 368) (prettyWithWater grid (S.map swap seen) source) $
          case gridM of
            -- Nothing -> goFromNewSourceAbove
            Nothing -> error "wat"
            Just grid' -> go grid' nextSeen nextSource nextWaterPositions nextSeenSourceWater
            -- Just grid' -> go grid' nextSeen (getSource grid' seen startPos Nothing) nextWaterPositions nextSeenSourceWater
      where
        (gridM, seen', waterPositions', nextSourceM) = dripDfs maxY grid source waterPositions seenSourceWater
        -- If we failed to drop, search for a new pos. Otherwise move to the last point of overflow
        nextSource = case gridM of
          Nothing -> getNextSource grid nextSeen source startPos
          Just _ -> case nextSourceM of
            Nothing -> source
            Just s -> s
        nextSeen = seen `S.union` S.map swap seen'
        nextWaterPositions = waterPositions `S.union` waterPositions'
        nextSeenSourceWater = S.insert (source, nextWaterPositions) seenSourceWater
        goFromNewSourceAbove = go grid seen (getSource grid nextSeen startPos (Just source)) nextWaterPositions nextSeenSourceWater

readGrid :: IO (Grid Cell, Coord2, Int)
readGrid = do
  veins <- readWithParser veins <$> input 2018 17
  -- veins <- readWithParser veins <$> exampleInput 2018 17
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
  let (grid', seen) = fillDfs maxY grid startPos
  putStrLn $ prettyWithWater grid' seen startPos
  return $ S.size (S.filter ((<= maxY) . snd) seen)
