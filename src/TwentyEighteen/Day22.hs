module TwentyEighteen.Day22 where

import Coord (Coord2, manhattan, neighborsNoDiags)
import Data.List (foldl', sort)
import Data.Map (Map)
import qualified Data.Map.Strict as M
import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.Set as S
import Data.Tuple.Extra (snd3, thd3)

depth :: Int
depth = 11109

target :: Coord2
target = (9, 731)

indexLevelRisk :: Map Coord2 (Int, Int, Int)
indexLevelRisk =
  foldl'
    f
    M.empty
    (sort [(x, y) | x <- [0 .. fst target * 20], y <- [0 .. snd target * 3]])
  where
    eLevel :: Int -> Int
    eLevel g = (g + depth) `mod` 20183
    getIndexLevelRisk :: Int -> (Int, Int, Int)
    getIndexLevelRisk g = let e = eLevel g in (g, e, e `mod` 3)
    f acc (0, 0) = M.insert (0, 0) (getIndexLevelRisk 0) acc
    f acc (x, y)
      | (x, y) == target = M.insert (x, y) (getIndexLevelRisk 0) acc
      | y == 0 = let g = x * 16807 in M.insert (x, y) (getIndexLevelRisk g) acc
      | x == 0 = let g = y * 48271 in M.insert (x, y) (getIndexLevelRisk g) acc
      | otherwise =
        let g = snd3 (acc M.! (x - 1, y)) * snd3 (acc M.! (x, y - 1))
         in M.insert (x, y) (getIndexLevelRisk g) acc

part1 :: Int
part1 =
  sum
    [ thd3 $ indexLevelRisk M.! (x, y)
      | x <- [0 .. fst target],
        y <- [0 .. snd target]
    ]

data RegionType = Rocky | Wet | Narrow deriving (Eq)

rType :: Coord2 -> RegionType
rType pos =
  case thd3 (indexLevelRisk M.! pos) of
    0 -> Rocky
    1 -> Wet
    2 -> Narrow

data Tool = Torch | ClimbingGear | Neither deriving (Eq, Ord, Show)

data ClimbState = ClimbState Coord2 Tool Int

startState :: ClimbState
startState = ClimbState (0, 0) Torch 0

nextStates :: ClimbState -> [ClimbState]
nextStates (ClimbState pos gear t) = moveStates ++ gearChangeStates
  where
    currentRType = rType pos
    allowedGearChange =
      case currentRType of
        Rocky -> [ClimbingGear, Torch]
        Wet -> [ClimbingGear, Neither]
        Narrow -> [Torch, Neither]
    allowedRTypes =
      case gear of
        Torch -> [Rocky, Narrow]
        ClimbingGear -> [Rocky, Wet]
        Neither -> [Wet, Narrow]
    ns =
      [ (x, y)
        | (x, y) <- neighborsNoDiags pos,
          x >= 0,
          y >= 0,
          rType (x, y) `elem` allowedRTypes
      ]
    moveStates = ClimbState <$> ns <*> pure gear <*> pure (t + 1)
    gearChangeStates = ClimbState pos <$> allowedGearChange <*> pure (t + 7)

heuristic :: ClimbState -> Int
heuristic (ClimbState pos gear t) =
  manhattan pos target + t + case gear of
    Torch -> 0
    _ -> 7

aStar :: Int
aStar = go (PQ.singleton (heuristic startState) startState) S.empty
  where
    go queue seen
      | pos == target && gear == Torch = t
      | (pos, gear) `S.member` seen = go rest seen
      | otherwise =
        go
          (foldl' (\q s -> PQ.insert (heuristic s) s q) rest (nextStates cs))
          (S.insert (pos, gear) seen)
      where
        ((_, cs@(ClimbState pos gear t)), rest) = PQ.deleteFindMin queue

part2 :: Int
part2 = aStar
