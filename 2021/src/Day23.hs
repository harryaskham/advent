{-# LANGUAGE QuasiQuotes #-}

module Day23 (part1, part2) where

import Data.Array qualified as A
import Data.Bimap (Bimap)
import Data.Bimap qualified as BM
import Data.List ((!!))
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Mod
import Data.PQueue.Prio.Min qualified as PQ
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
import Helper.Tracers (traceWhen)
import Helper.Util
import Text.ParserCombinators.Parsec
import Prelude hiding (find)

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

destinations :: Amphipod -> [Coord2]
destinations Amber = [(3, 2), (3, 3)]
destinations Bronze = [(5, 2), (5, 3)]
destinations Copper = [(7, 2), (7, 3)]
destinations Desert = [(9, 2), (9, 3)]

energy :: Cell -> Int
energy (Full Amber) = 1
energy (Full Bronze) = 10
energy (Full Copper) = 100
energy (Full Desert) = 1000

organized :: Grid Cell -> Bool
organized g =
  all
    (\a -> length (find (Full a) g `L.intersect` destinations a) == 2)
    enumerate

illegalStops :: [Coord2]
illegalStops = [(3, 1), (5, 1), (7, 1), (9, 1)]

getRoom :: Coord2 -> Maybe (Amphipod, [Coord2])
getRoom c
  | c `elem` destinations Amber = Just (Amber, destinations Amber)
  | c `elem` destinations Bronze = Just (Bronze, destinations Bronze)
  | c `elem` destinations Copper = Just (Copper, destinations Copper)
  | c `elem` destinations Desert = Just (Desert, destinations Desert)
  | otherwise = Nothing

-- Can this amphipod move to this position?
-- Not the hallway condition, only the room one
validMove :: Grid Cell -> Cell -> Coord2 -> Coord2 -> Bool
validMove g a p n
  | (g M.! n) /= Empty = False
  | otherwise =
    case getRoom n of
      Nothing -> True
      Just (a', ps) ->
        p `elem` ps -- if we're already in the room, doesn't matter if it's ours, we can move
          || ( a == Full a' -- then we're hallway, so if it's a room, it's the right room, and also it's got the right ones in it
                 && length [a'' | a'' <- (g M.!) <$> ps, a'' `elem` [Empty, a]] == 2
             )

makeMove :: (Coord2, Coord2) -> Grid Cell -> Grid Cell
makeMove (a, b) g = M.insert a Empty . M.insert b (g M.! a) $ g

-- If an apod just moved, then we're still "in its move" and it can stop in the hallway
-- We should store whether apod is in its move and whetehr it started from a hallway
organize :: Grid Cell -> Maybe Int
organize g' = go (PQ.singleton 0 (g', 0, Nothing)) S.empty
  where
    go queue seen
      | PQ.null queue = Nothing
      | organized g = Just pathCost
      | g `S.member` seen = go rest seen
      | otherwise =
        -- pauseId $
        traceWhen (g M.! (9, 3) == Empty) (traceShow "EMPTY") $
          traceShow (state, pathCost, h g, cost) $
            traceTextLn (pretty g) $
              go queue' seen'
      where
        seen' = S.insert g seen
        -- can do better by using an actual distance map / just using topology
        h g =
          sum
            [ energy (Full a)
                * sum
                  [ L.minimum [manhattan p d | d <- destinations a]
                    | p <- find (Full a) g
                  ]
              | a <- enumerate
            ]
        ((cost, (g, pathCost, state)), rest) = PQ.deleteFindMin queue
        illegallyOccupied = case [p | p <- illegalStops, (g M.! p) /= Empty] of
          [] -> []
          [p] -> [p]
          _ -> error "multiple illegals"
        movesFor a =
          [ (makeMove (p, n) g, pathCost + energy (Full a), updateState state p n)
            | p <- find (Full a) g,
              n <- neighborsNoDiags p,
              validMove g (Full a) p n
          ]
        nullMove = (g, pathCost, Nothing)
        allMoves = nullMove : (movesFor =<< [Amber, Bronze, Copper, Desert])
        -- keep track of whether we just moved the same one, or a new one
        updateState :: (Maybe (Coord2, Coord2)) -> Coord2 -> Coord2 -> Maybe (Coord2, Coord2)
        updateState Nothing p n = Just (n, p)
        updateState (Just (lastPos, origin)) p n
          | p == lastPos = Just (n, origin)
          | otherwise = Just (n, p)
        nextStates :: [(Grid Cell, Int, Maybe (Coord2, Coord2))]
        nextStates =
          if not (null illegallyOccupied)
            then -- always move illegals first

              traceShow ("illegal occupation, must move:", illegallyOccupied, state) $
                movingOne (L.head illegallyOccupied)
            else -- TODO: if we're moving an apod that started in the hallway, wemust keep moving that apod
            case state of
              Nothing ->
                traceShow ("no state, so any move allowed") $
                  allMoves
              Just (lastPos, origin) ->
                case getRoom origin of
                  Just _ ->
                    traceShow ("current mover started in a room, any move allowed") $
                      allMoves -- we started in a room so free to stop anywhere
                  Nothing ->
                    traceShow ("current mover started the hall") $
                      -- we started in the hallway so we have to stop in a room
                      case getRoom lastPos of
                        Just _ ->
                          traceShow ("current hallway mover made it to a room so that's okay") $
                            allMoves -- our last-moved landed in a room so that's fine
                        Nothing ->
                          traceShow ("current hallway mover still in hall so need to move him") $
                            movingOne lastPos -- our last moved is going hallway to hallway and needs to move
        movingOne p =
          [ (makeMove (p, n) g, pathCost + energy a, updateState state p n)
            | let a = g M.! p,
              n <- neighborsNoDiags p,
              validMove g a p n
          ]
        queue' = foldl' (\q st@(g, pathCost, _) -> PQ.insert (pathCost + h g) st q) rest nextStates

-- okay so:
-- hardcode win positions
-- can leave an Apod in an invalid position but then there's only one legal move, to move it away
-- only move to room if room is dest && there are no others in it
-- precompute all-points all-apods paths?
-- a* search?

testInput =
  [s|
#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########
|]

testInput2 =
  [s|
#############
#.....D.D.A.#
###.#B#C#.###
  #A#B#C#.#
  #########
|]

testInput3 =
  [s|
#############
#.....D.....#
###.#B#C#D###
  #A#B#C#A#
  #########
|]

part1 :: Maybe Int
part1 =
  --(readGrid $(input 23) :: Grid Cell)
  (readGrid $(input 23) :: Grid Cell)
    --(readGrid $(exampleInputN 23 1) :: Grid Cell)
    --(readGrid testInput3 :: Grid Cell)
    & fillDef None
    & organize

part2 :: Text
part2 = "Part 2"
