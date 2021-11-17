module TwentyNineteen.Day20 where

import Coord (Coord2, neighborsNoDiags)
import Data.List ((\\))
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import qualified Data.Sequence as SQ
import qualified Data.Set as S
import Grid (Grid, maxXY, toGrid)
import Util (input, unjust, (<$$>))

data Cell = Empty | Wall | Blank | Label Char deriving (Eq, Ord)

fromChar :: Char -> Cell
fromChar '.' = Empty
fromChar '#' = Wall
fromChar ' ' = Blank
fromChar c = Label c

isLabel :: Cell -> Bool
isLabel (Label _) = True
isLabel _ = False

getLabel :: Cell -> Char
getLabel (Label c) = c

portalPositions :: Grid Cell -> Map String [Coord2]
portalPositions grid =
  M.fromListWith
    (++)
    [ (unjust l, [(x, y)])
      | x <- [2 .. maxX -2],
        y <- [2 .. maxY -2],
        let l = label (x, y),
        isJust l
    ]
  where
    (maxX, maxY) = maxXY grid
    label (x, y)
      | M.lookup (x, y) grid /= Just Empty = Nothing
      | null labelCandidates = Nothing
      | otherwise = Just $ fmap getLabel . head $ labelCandidates
      where
        labelCandidates =
          filter (all isLabel) $
            (grid M.!)
              <$$> [ [(x + 1, y), (x + 2, y)],
                     [(x -2, y), (x -1, y)],
                     [(x, y + 1), (x, y + 2)],
                     [(x, y -2), (x, y -1)]
                   ]

type Solver = Grid Cell -> Map String [Coord2] -> Map Coord2 String -> String -> String -> Maybe Int

pathLength :: Solver
pathLength grid pps posPortals start end =
  go (SQ.singleton ((head $ pps M.! start), 0)) S.empty
  where
    target = head $ pps M.! end
    go SQ.Empty _ = Nothing
    go ((current, steps) SQ.:<| queue) seen
      | current == target = Just steps
      | current `S.member` seen = go queue seen
      | otherwise = go nextQueue (S.insert current seen)
      where
        nextStates = [(n, steps + 1) | n <- neighborsNoDiags current, grid M.! n == Empty]
        portalState = case M.lookup current posPortals of
          Nothing -> []
          Just portal -> (\c -> (c, steps + 1)) <$> ((pps M.! portal) \\ [current])
        nextQueue = queue SQ.>< SQ.fromList (nextStates ++ portalState)

solve :: Solver -> IO (Maybe Int)
solve f = do
  grid <- toGrid fromChar . lines <$> input 2019 20
  let pps = portalPositions grid
      posPortals = M.fromList [(c, p) | (p, cs) <- M.toList pps, c <- cs]
  return $ f grid pps posPortals "AA" "ZZ"

part1 :: IO (Maybe Int)
part1 = solve pathLength

recursivePathLength :: Solver
recursivePathLength grid pps posPortals start end =
  go (SQ.singleton ((head $ pps M.! start), 0, 0)) S.empty
  where
    target = (head $ pps M.! end, 0)
    go SQ.Empty _ = Nothing
    go ((current@(x, y), steps, level) SQ.:<| queue) seen
      | (current, level) == target = Just steps
      | (current, level) `S.member` seen = go queue seen
      | otherwise = go nextQueue (S.insert (current, level) seen)
      where
        (maxX, maxY) = maxXY grid
        outerPortal = x == 2 || x == maxX - 2 || y == 2 || y == maxY - 2
        nextStates = [(n, steps + 1, level) | n <- neighborsNoDiags current, grid M.! n == Empty]
        portalState =
          if level > 0 || not outerPortal
            then case M.lookup current posPortals of
              Nothing -> []
              Just portal ->
                (\c -> (c, steps + 1, if outerPortal then level - 1 else level + 1))
                  <$> ((pps M.! portal) \\ [current])
            else []
        nextQueue = queue SQ.>< SQ.fromList (nextStates ++ portalState)

part2 :: IO (Maybe Int)
part2 = solve recursivePathLength
