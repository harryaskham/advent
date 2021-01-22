{-# LANGUAGE TupleSections #-}

module TwentyEighteen.Day17 where

import Coord (Coord2, neighborsNoDiags)
import Data.List (foldl')
import Data.List.Extra (maximumOn, minimumOn)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import qualified Data.Sequence as SQ
import Data.Set (Set)
import qualified Data.Set as S
import Grid (Grid, maxXY, minXY, pretty)
import Text.ParserCombinators.Parsec
  ( GenParser,
    char,
    digit,
    eof,
    many,
    many1,
    oneOf,
    string,
  )
import Util (eol, input, readWithParser)

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

dripBfs :: Int -> Grid Cell -> Coord2 -> (Grid Cell, Set Coord2)
dripBfs maxY grid source =
  go (SQ.singleton (source, S.empty)) grid S.empty
  where
    go SQ.Empty grid allSeen = (grid, allSeen)
    go ((pos@(x, y), seen) SQ.:<| queue) grid allSeen
      | not (pos `M.member` grid) = continue
      | grid M.! pos == Water = continue
      | pos `S.member` seen = continue
      | null nextStates && closedIn grid (x, y) = continueWithWater
      | otherwise = continue
      where
        continue = go nextQueue grid nextAllSeen
        continueWithWater = go queueFromWater (M.insert pos Water grid) nextAllSeen
        nextAllSeen = S.insert pos allSeen
        nextSeen = S.insert pos seen
        mkState seen pos =
          if (not (pos `S.member` seen)) && M.lookup pos grid == Just Sand
            then Just pos
            else Nothing
        downState = mkState seen (x, y + 1)
        leftState = if y < maxY then mkState seen (x - 1, y) else Nothing
        rightState = if y < maxY then mkState seen (x + 1, y) else Nothing
        nextPositions = case downState of
          Just ds -> [ds]
          Nothing -> catMaybes [leftState, rightState]
        nextStates = (,nextSeen) <$> nextPositions
        fromWaterStates =
          (,S.empty)
            <$> catMaybes
              [ mkState S.empty n
                | n <- neighborsNoDiags pos,
                  n `S.member` nextAllSeen
              ]
        nextQueue = queue SQ.>< SQ.fromList nextStates
        queueFromWater = queue SQ.>< SQ.fromList fromWaterStates

readGrid :: IO (Grid Cell, Coord2, Int, Int)
readGrid = do
  veins <- readWithParser veins <$> input 2018 17
  let clayCoords = S.fromList $ concat veins
      minX = fst $ minimumOn fst (concat veins)
      maxX = fst $ maximumOn fst (concat veins)
      minY = snd $ minimumOn snd (concat veins)
      maxY = snd $ maximumOn snd (concat veins)
      startPos = (500 - minX + 1, 0)
      grid =
        M.fromList
          [ ((x, y), if (x + minX - 1, y) `S.member` clayCoords then Clay else Sand)
            | x <- [0 .. (maxX - minX) + 2],
              y <- [0 .. maxY]
          ]
  return (grid, startPos, minY, maxY)

prettyWithWater :: Grid Cell -> Set Coord2 -> Coord2 -> String
prettyWithWater grid seen source =
  pretty $
    M.insert source Source $
      foldl'
        (\g p -> if g M.! p /= Water then M.insert p SeenWater g else g)
        grid
        (S.toList seen')
  where
    (minX, minY) = minXY grid
    (maxX, maxY) = maxXY grid
    seen' = S.filter (\(x, y) -> x >= minX && x <= maxX && y >= minY && y <= maxY) seen

part12 :: IO (Int, Int)
part12 = do
  (grid, startPos, minY, maxY) <- readGrid
  let (grid', seen) = dripBfs maxY grid startPos
  putStrLn $ prettyWithWater grid' seen startPos
  return
    ( S.size (S.filter (\(_, y) -> y >= minY && y <= maxY) seen),
      M.size $ M.filter (== Water) grid'
    )
