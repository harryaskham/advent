module Day23 where

import Data.Bimap (Bimap)
import Data.Bimap qualified as BM
import Data.Foldable (foldl1)
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Hashable (Hashable)
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

type APos = HashMap Amphipod (HashSet Coord2)

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

energy :: Map Amphipod Int
energy = M.fromList [(Amber, 1), (Bronze, 10), (Copper, 100), (Desert, 1000)]

positions :: Grid Cell -> APos
positions g = HM.fromList [(a, HS.fromList $ find (Full a) g) | a <- enumerate]

organized :: APos -> Bool
organized aPos = all (\a -> aPos HM.! a `HS.isSubsetOf` (destinationMap HM.! a)) enumerate

destinationMap :: HashMap Amphipod (HashSet Coord2)
destinationMap =
  HM.fromList
    [ (Amber, HS.fromList $ (3,) <$> [2 .. 5]),
      (Bronze, HS.fromList $ (5,) <$> [2 .. 5]),
      (Copper, HS.fromList $ (7,) <$> [2 .. 5]),
      (Desert, HS.fromList $ (9,) <$> [2 .. 5])
    ]

validHallwayDestinations :: HashSet Coord2
validHallwayDestinations = HS.fromList $ (,1) <$> [1, 2, 4, 6, 8, 10, 11]

allPaths :: HashMap (Coord2, Coord2) [Coord2]
allPaths =
  let allPos = HS.toList $ foldl1 HS.union (HM.elems destinationMap) `HS.union` validHallwayDestinations
   in HM.fromList [((a, b), L.nub . L.delete a $ pathFrom a b) | a <- allPos, b <- allPos]

pathFrom :: Coord2 -> Coord2 -> [Coord2]
pathFrom (x, y) (dx, dy)
  | (x, y) == (dx, dy) = []
  | x == dx = [(x, y') | y' <- [1 .. dy]]
  | y > 1 && x == dx = [(dx, y') | y' <- [min y dy .. max y dy]]
  | y > 1 && x /= dx = [(x, y') | y' <- [1 .. y]] ++ pathFrom (x, 1) (dx, dy)
  | y == 1 = [(x', 1) | x' <- [min x dx .. max x dx]] ++ pathFrom (dx, 1) (dx, dy)

allowedDestinationsPaths :: Amphipod -> Coord2 -> APos -> [(Coord2, Int)]
allowedDestinationsPaths a origin@(_, oy) aPos = mapMaybe (pathCost origin) (HS.toList validDestinations)
  where
    allAPos = foldl1 HS.union (HM.elems aPos)
    otherAPos = foldl1 HS.union [aPos HM.! a' | a' <- enumerate, a /= a']
    hallWayOrigin = oy == 1
    validRoomDestinations =
      let others = otherAPos `HS.intersection` (destinationMap HM.! a)
       in if null others
            then destinationMap HM.! a `HS.difference` allAPos
            else HS.empty
    validDestinations =
      if hallWayOrigin
        then validRoomDestinations
        else validHallwayDestinations `HS.union` validRoomDestinations
    accessibleFrom current dest =
      let path = allPaths HM.! (current, dest)
       in if any (`HS.member` allAPos) path then Nothing else Just path
    pathCost origin dest =
      case accessibleFrom origin dest of
        Nothing -> Nothing
        Just path -> Just (dest, length path * energy M.! a)

allowedDestinationsBfs :: Grid Cell -> Amphipod -> Coord2 -> APos -> [(Coord2, Int)]
allowedDestinationsBfs g a origin@(_, oy) aPos = go (SQ.singleton (origin, 0)) S.empty []
  where
    allAPos = foldl1 HS.union (HM.elems aPos)
    otherAPos = foldl1 HS.union [ps | (a', ps) <- HM.toList aPos, a /= a']
    hallWayOrigin = oy == 1
    validRoomDestinations =
      let others = otherAPos `HS.intersection` (destinationMap HM.! a)
       in if null others
            then destinationMap HM.! a `HS.difference` allAPos
            else HS.empty
    validDestinations = validHallwayDestinations `HS.union` validRoomDestinations
    go SQ.Empty _ destinations = destinations
    go ((p, cost) SQ.:<| rest) seen destinations
      | p `S.member` seen = go rest seen destinations
      | hallWayOrigin && p `HS.member` validRoomDestinations = go queue seen' ((p, cost) : destinations)
      | not hallWayOrigin && p `HS.member` validDestinations = go queue seen' ((p, cost) : destinations)
      | otherwise = go queue seen' destinations
      where
        seen' = S.insert p seen
        nextStates = [(n, cost + energy M.! a) | n <- neighborsNoDiags p, g M.! n /= Wall, not (n `HS.member` allAPos)]
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
        move p d a aPos = HM.adjust (HS.delete p . HS.insert d) a aPos
        states =
          [ (aPos', pathCost + dist)
            | (a, ps) <- HM.toList aPos,
              p <- HS.toList ps,
              (d, dist) <- allowedDestinationsBfs g a p aPos,
              -- (d, dist) <- allowedDestinationsPaths a p aPos,
              let aPos' = move p d a aPos
          ]
        minDistanceToDest (x, y) a
          | x == dx && y > 1 = 0
          | x == dx && y == 1 = 1
          | otherwise = y - 1 + abs (x - dx) + 1
          where
            dx = fst . L.head . HS.toList $ destinationMap HM.! a
        h aPos = sum [energy M.! a * minDistanceToDest p a | a <- enumerate, p <- HS.toList (aPos HM.! a)]
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
