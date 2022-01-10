module Day23 where

import Control.Lens.Combinators (none)
import Data.Bimap (Bimap)
import Data.Bimap qualified as BM
import Data.Foldable (foldl1)
import Data.HashSet qualified as HS
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.PQueue.Prio.Min qualified as PQ
import Data.Sequence qualified as SQ
import Data.Set qualified as S
import Data.String.QQ (s)
import Data.Text qualified as T
import Data.Vector qualified as V
import Helper.Coord (Coord2, neighborsNoDiags)
import Helper.Grid (Grid, GridCell (charMap), fillDef, find, readGrid)
import Helper.Tracers
import Helper.Util (enumerate)
import Prelude hiding (find)

maze1 :: Text
maze1 =
  [s|
#############
#...........#
###B#B#D#D###
  #C#C#A#A#
  #########
  #########
  #########
|]

maze2 :: Text
maze2 =
  [s|
#############
#...........#
###B#B#D#D###
  #D#C#B#A#
  #D#B#A#C#
  #C#C#A#A#
  #########
|]

data Amphipod = Amber | Bronze | Copper | Desert deriving (Eq, Ord, Show, Enum, Generic)

instance Hashable Amphipod

type APos = Map Amphipod (Set Coord2)

data Cell
  = None
  | Wall
  | Empty
  | Full Amphipod
  deriving (Eq, Ord)

instance GridCell Cell where
  charMap =
    BM.fromList
      [ (None, ' '),
        (Wall, '#'),
        (Empty, '.'),
        (Full Amber, 'A'),
        (Full Bronze, 'B'),
        (Full Copper, 'C'),
        (Full Desert, 'D')
      ]

energy :: Amphipod -> Int
energy Amber = 1
energy Bronze = 10
energy Copper = 100
energy Desert = 1000

positions :: Grid Cell -> APos
positions g = M.fromList [(a, S.fromList $ find (Full a) g) | a <- enumerate]

organized :: APos -> Bool
organized aPos = all (\a -> aPos M.! a `S.isSubsetOf` (destinationMap M.! a)) enumerate

destinationMap :: Map Amphipod (Set Coord2)
destinationMap =
  M.fromList
    [ (Amber, S.fromList $ (3,) <$> [2 .. 5]),
      (Bronze, S.fromList $ (5,) <$> [2 .. 5]),
      (Copper, S.fromList $ (7,) <$> [2 .. 5]),
      (Desert, S.fromList $ (9,) <$> [2 .. 5])
    ]

validHallwayDestinations :: Set Coord2
validHallwayDestinations = S.fromList $ (,1) <$> [1, 2, 4, 6, 8, 10, 11]

allowedDestinationsPaths :: Amphipod -> Coord2 -> APos -> [(Coord2, Int)]
allowedDestinationsPaths a origin@(_, oy) aPos = mapMaybe (pathCost origin) (S.toList validDestinations)
  where
    allAPos = foldl1 S.union (M.elems aPos)
    otherAPos = foldl1 S.union [ps | (a', ps) <- M.toList aPos, a /= a']
    hallWayOrigin = oy == 1
    validRoomDestinations =
      let others = otherAPos `S.intersection` (destinationMap M.! a)
       in if null others
            then destinationMap M.! a S.\\ allAPos
            else S.empty
    validDestinations =
      if hallWayOrigin
        then validRoomDestinations
        else validHallwayDestinations `S.union` validRoomDestinations
    accessibleFrom current dest =
      let path = allPaths M.! (current, dest)
       in if none (`S.member` allAPos) path
            then Just path
            else Nothing
    pathCost origin dest =
      case accessibleFrom origin dest of
        Nothing -> Nothing
        Just path -> Just (dest, length path * energy a)

allPaths :: Map (Coord2, Coord2) [Coord2]
allPaths =
  let allPos = S.toList $ foldl1 S.union (M.elems destinationMap) `S.union` validHallwayDestinations
   in M.fromList [((a, b), L.nub . L.delete a $ pathFrom a b) | a <- allPos, b <- allPos]

pathFrom :: Coord2 -> Coord2 -> [Coord2]
pathFrom (x, y) (dx, dy)
  | (x, y) == (dx, dy) = []
  | x == dx = [(x, y') | y' <- [1 .. dy]]
  | y > 1 && x == dx = [(dx, y') | y' <- [min y dy .. max y dy]]
  | y > 1 && x /= dx = [(x, y') | y' <- [1 .. y]] ++ pathFrom (x, 1) (dx, dy)
  | y == 1 = [(x', 1) | x' <- [min x dx .. max x dx]] ++ pathFrom (dx, 1) (dx, dy)

allowedDestinationsBfs :: Grid Cell -> Amphipod -> Coord2 -> APos -> [(Coord2, Int)]
allowedDestinationsBfs g a origin@(_, oy) aPos = go (SQ.singleton (origin, 0)) S.empty []
  where
    allAPos = foldl1 S.union (M.elems aPos)
    otherAPos = foldl1 S.union [ps | (a', ps) <- M.toList aPos, a /= a']
    hallWayOrigin = oy == 1
    validRoomDestinations =
      let others = otherAPos `S.intersection` (destinationMap M.! a)
       in if null others
            then destinationMap M.! a S.\\ allAPos
            else S.empty
    validDestinations = validHallwayDestinations `S.union` validRoomDestinations
    go SQ.Empty _ destinations = destinations
    go ((p, cost) SQ.:<| rest) seen destinations
      | p `S.member` seen = go rest seen destinations
      | hallWayOrigin && p `S.member` validRoomDestinations = go queue seen' ((p, cost) : destinations)
      | not hallWayOrigin && p `S.member` validDestinations = go queue seen' ((p, cost) : destinations)
      | otherwise = go queue seen' destinations
      where
        seen' = S.insert p seen
        nextStates = [(n, cost + energy a) | n <- neighborsNoDiags p, g M.! n /= Wall, not (n `S.member` allAPos)]
        queue = rest SQ.>< SQ.fromList nextStates

solve :: Grid Cell -> Maybe Int
solve g = go (PQ.singleton 0 (positions g, 0)) HS.empty
  where
    go queue seen
      | PQ.null queue = Nothing
      | organized aPos = Just pathCost
      | aPos `HS.member` seen = go rest seen
      | otherwise = traceShow (pathCost, cost) $ go queue' seen'
      where
        ((cost, (aPos, pathCost)), rest) = PQ.deleteFindMin queue
        seen' = HS.insert aPos seen
        move p d a aPos = M.adjust (S.delete p . S.insert d) a aPos
        states =
          [ (aPos', pathCost + dist)
            | (a, ps) <- M.toList aPos,
              p <- S.toList ps,
              --(d, dist) <- allowedDestinationsBfs g a p aPos,
              (d, dist) <- allowedDestinationsPaths a p aPos,
              let aPos' = move p d a aPos
          ]
        minDistanceToDest (x, y) a
          | x == dx && y > 1 = 0
          | x == dx && y == 1 = 1
          | otherwise = y - 1 + abs (x - dx) + 1
          where
            dx = fst . L.head . S.toList $ destinationMap M.! a
        h aPos = sum [energy a * minDistanceToDest p a | a <- enumerate, p <- S.toList (aPos M.! a)]
        queue' = foldl' (\q st@(aPos', pathCost') -> PQ.insert (pathCost' + h aPos') st q) rest states

part1 :: Maybe Int
part1 =
  --const (Just 10411) $
  (readGrid maze1 :: Grid Cell)
    & fillDef None
    & solve

part2 :: Maybe Int
part2 =
  --const (Just 46721) $
  (readGrid maze2 :: Grid Cell)
    & fillDef None
    & solve
