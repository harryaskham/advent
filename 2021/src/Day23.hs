{-# LANGUAGE QuasiQuotes #-}

-- TODO:
-- reenable tests

module Day23 where

import Data.Array qualified as A
import Data.Bimap (Bimap)
import Data.Bimap qualified as BM
import Data.List ((!!))
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Mod
import Data.PQueue.Prio.Min qualified as PQ
import Data.PSQueue qualified as PS
import Data.Sequence qualified as SQ
import Data.Set qualified as S
import Data.String.QQ
import Data.Text qualified as T
import Data.Text.Read
import Data.Vector qualified as V
import Helper.Coord
import Helper.Grid hiding (Empty, Wall)
import Helper.TH
import Helper.Tracers
import Helper.Util
import Text.ParserCombinators.Parsec hiding (updateState)
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

data Amphipod = Amber | Bronze | Copper | Desert deriving (Eq, Ord, Show, Enum)

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

validHallwayDestinations :: Set Coord2
validHallwayDestinations = S.fromList $ (,1) <$> [1, 2, 4, 6, 8, 9, 10, 11]

-- TODO: memo this
allowedDestinations :: Grid Cell -> Amphipod -> Coord2 -> Map Amphipod [Coord2] -> [(Coord2, Int)]
allowedDestinations g a origin@(ox, oy) aPos = go (SQ.singleton (origin, 0)) S.empty []
  where
    allAPos = S.fromList (concat (M.elems aPos)) -- faster
    otherAPos = S.fromList ([p | (a', ps) <- M.toList aPos, a /= a', p <- ps]) -- faster
    hallWayOrigin = oy == 1
    validRoomDestinations = S.fromList $ let ds = destinations a in if any (`S.member` otherAPos) ds then [] else ds
    validDestinations = validHallwayDestinations `S.union` validRoomDestinations
    go SQ.Empty _ destinations = destinations
    go ((p, cost) SQ.:<| rest) seen destinations
      | p `S.member` seen = go rest seen destinations
      | hallWayOrigin && p `S.member` validRoomDestinations = go queue seen' ((p, cost) : destinations)
      | (not hallWayOrigin) && p `S.member` validDestinations = go queue seen' ((p, cost) : destinations)
      | otherwise = go queue seen' destinations
      where
        seen' = S.insert p seen
        nextStates = [(n, cost + energy a) | n <- neighborsNoDiags p, g M.! n /= Wall, not (n `S.member` allAPos)]
        queue = rest SQ.>< SQ.fromList nextStates

emptyG :: Text
emptyG =
  [s|
#############
#...........#
###.#.#.#.###
  #.#.#.#.#
  #########
  #########
  #########
|]

prettyA aPos =
  pretty $
    foldl'
      ( \g (a, ps) ->
          foldl' (\g p -> M.insert p (Full a) g) g ps
      )
      (fillDef None $ readGrid emptyG)
      (M.toList aPos)

solve :: Grid Cell -> Maybe Int
solve g = go (PQ.singleton 0 (positions g, 0)) S.empty
  where
    go queue seen
      | PQ.null queue = Nothing
      | aPos `S.member` seen = go rest seen
      | organized aPos = Just pathCost
      | otherwise =
        traceShow ("pathCost", pathCost, "pq cost", cost, "q size", PQ.size queue, "seen size", S.size seen) $
          -- traceTextLn (prettyA aPos)
          go queue' seen'
      where
        ((cost, (aPos, pathCost)), rest) = PQ.deleteFindMin queue
        seen' = S.insert aPos seen
        move p d a aPos = M.adjust (L.delete p . (d :)) a aPos
        states =
          [ (aPos', pathCost + dist)
            | (a, ps) <- M.toList aPos,
              p <- ps,
              (d, dist) <- allowedDestinations g a p aPos,
              let aPos' = move p d a aPos
          ]
        minDistanceToDest' (x, y) a = abs (dx - x)
          where
            ((dx, _) : _) = destinations a
        minDistanceToDest (x, y) a
          | x == dx && y > 1 = 0
          | x == dx && y == 1 = 1
          | otherwise = (y - 1) + abs (x - dx)
          where
            ((dx, _) : _) = destinations a
        --h aPos = sum [energy a * minDistanceToDest p a | a <- enumerate, p <- aPos M.! a]
        h aPos = sum [energy a * minDistanceToDest' p a | a <- enumerate, p <- aPos M.! a]
        --h _ = 0
        queue' = foldl' (flip (PQ.insert (pathCost + h aPos))) rest states

destinations :: Amphipod -> [Coord2]
destinations Amber = (3,) <$> [2 .. 5]
destinations Bronze = (5,) <$> [2 .. 5]
destinations Copper = (7,) <$> [2 .. 5]
destinations Desert = (9,) <$> [2 .. 5]

positions :: Grid Cell -> Map Amphipod [Coord2]
positions g = M.fromList [(a, find (Full a) g) | a <- enumerate]

energy :: Amphipod -> Int
energy Amber = 1
energy Bronze = 10
energy Copper = 100
energy Desert = 1000

organized :: Map Amphipod [Coord2] -> Bool
organized aPos =
  all
    (\a -> (aPos M.! a) `L.intersect` destinations a == (aPos M.! a))
    enumerate

exx =
  [s|
#############
#...........#
###B#C#B#D###
  #D#C#B#A#
  #D#B#A#C#
  #A#D#C#A#
  #########
|]

exx2 =
  [s|
#############
#.A...B.....#
###.#.#C#D###
  #A#B#C#D#
  #A#B#C#D#
  #A#B#C#D#
  #########
|]

-- completes
exx3 =
  [s|
#############
#A........BD#
###B#C#.#.###
  #D#C#B#.#
  #D#B#A#C#
  #A#D#C#A#
  #########
|]

exx4 =
  [s|
#############
#A.........D#
###B#C#B#.###
  #D#C#B#.#
  #D#B#A#C#
  #A#D#C#A#
  #########
|]

-- should complete fast
exx5 =
  [s|
#############
#AA.....B.BD#
###B#.#.#.###
  #D#C#.#.#
  #D#B#C#C#
  #A#D#C#A#
  #########
|]

exx6 =
  [s|
#############
#.........AD#
###.#B#C#.###
  #A#B#C#D#
  #A#B#C#D#
  #A#B#C#D#
  ######### 
|]

exx7 =
  [s|
#############
#AA.D.....AD#
###.#B#C#.###
  #.#B#C#.#
  #.#B#C#D#
  #A#B#C#D#
  #########
|]

part1 =
  (readGrid maze1 :: Grid Cell)
    & fillDef None
    & solve

part2 =
  (readGrid maze2 :: Grid Cell)
    & fillDef None
    & solve
