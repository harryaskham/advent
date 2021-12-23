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

destinations :: Amphipod -> [Coord2]
destinations Amber = (3,) <$> [2 .. 5]
destinations Bronze = (5,) <$> [2 .. 5]
destinations Copper = (7,) <$> [2 .. 5]
destinations Desert = (9,) <$> [2 .. 5]

positions :: Grid Cell -> Map Amphipod [Coord2]
positions g = M.fromList [(a, find (Full a) g) | a <- enumerate]
  
energy :: Cell -> Int
energy (Full Amber) = 1
energy (Full Bronze) = 10
energy (Full Copper) = 100
energy (Full Desert) = 1000

organized :: Grid Cell -> Map Amphipod [Coord2] -> Bool
organized g aPos =
  all
    (\a -> (aPos M.! a) `L.intersect` destinations a == (aPos M.! a))
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
                 && length [a'' | a'' <- (g M.!) <$> ps, a'' `elem` [Empty, a]] == (length [p | p <- ps, g M.! p /= Wall])
             )

makeMove :: (Coord2, Coord2) -> Grid Cell -> Grid Cell
makeMove (a, b) g = M.insert a Empty . M.insert b (g M.! a) $ g

-- If an apod just moved, then we're still "in its move" and it can stop in the hallway
-- We should store whether apod is in its move and whetehr it started from a hallway
organize :: Grid Cell -> Maybe Int
organize g' = go (PQ.singleton 0 (g', (positions g'), 0, Nothing)) S.empty
  where
    go queue seen
      | PQ.null queue = Nothing
      | organized g aPos = Just pathCost
      | g `S.member` seen = go rest seen
      | otherwise =
        traceShow (pathCost, cost) $
          traceWhen
            debug
            ( pauseId $
                traceWhen (g M.! (9, 3) == Empty) (traceShow "EMPTY") $
                  traceShow (state, pathCost, h g, cost) $
                    traceTextLn (pretty g)
            )
            $ go queue' seen'
      where
        ((cost, (g, aPos, pathCost, state)), rest) = PQ.deleteFindMin queue
        seen' = S.insert g seen
        -- can do better by using an actual distance map / just using topology
        -- if we're in the destination then the cost is the number of empties below us
        -- or just as before, but put penalties on empty spaces
        roomCost g = sum [energy (Full a) * (dy - 2) | a <- enumerate, let ds = destinations a, d@(dx, dy) <- ds, g M.! d /= Full a]
        minDistanceToDest p@(x, y) a
          | x == dx = y - 1
          | otherwise = (y - 1) + L.minimum [manhattan p d | d <- ds, g M.! d /= (Full a)]
          where
            ds@((dx, _) : _) = destinations a
        minDistanceToDest' p a = L.minimum (manhattan p <$> destinations a)
        h g = sum [energy (Full a) * minDistanceToDest' p a | a <- enumerate, p <- aPos M.! a]
        illegallyOccupied = case [p | p <- illegalStops, (g M.! p) /= Empty] of
          [] -> []
          [p] -> [p]
          _ -> error "multiple illegals"
        movesFor a =
          [ (makeMove (p, n) g, M.adjust (L.delete p . (n:)) a aPos, pathCost + energy (Full a), updateState state p n)
            | p <- aPos M.! a,
              n <- neighborsNoDiags p,
              validMove g (Full a) p n
          ]
        allMoves = movesFor =<< [Amber, Bronze, Copper, Desert]
        -- keep track of whether we just moved the same one, or a new one
        updateState :: (Maybe (Coord2, Coord2)) -> Coord2 -> Coord2 -> Maybe (Coord2, Coord2)
        updateState Nothing p n = Just (n, p)
        updateState (Just (lastPos, origin)) p n
          | p == lastPos = Just (n, origin)
          | otherwise = Just (n, p)
        nextStates :: [(Grid Cell, Map Amphipod [Coord2], Int, Maybe (Coord2, Coord2))]
        nextStates =
          if not (null illegallyOccupied)
            then -- always move illegals first

              traceWhen debug (traceShow ("illegal occupation, must move:", illegallyOccupied, state)) $
                movingOne (L.head illegallyOccupied)
            else -- TODO: if we're moving an apod that started in the hallway, wemust keep moving that apod
            case state of
              Nothing ->
                traceWhen debug (traceShow ("no state, so any move allowed")) $
                  allMoves
              Just (lastPos, origin) ->
                case getRoom origin of
                  Just _ ->
                    traceWhen debug (traceShow ("current mover started in a room, any move allowed")) $
                      allMoves -- we started in a room so free to stop anywhere
                  Nothing ->
                    traceWhen debug (traceShow ("current mover started the hall")) $
                      -- we started in the hallway so we have to stop in a room
                      case getRoom lastPos of
                        Just _ ->
                          traceWhen debug (traceShow ("current hallway mover made it to a room so that's okay")) $
                            allMoves -- our last-moved landed in a room so that's fine
                        Nothing ->
                          traceWhen debug (traceShow ("current hallway mover still in hall so need to move him")) $
                            movingOne lastPos -- our last moved is going hallway to hallway and needs to move
        movingOne p =
          [ (makeMove (p, n) g, M.adjust (L.delete p . (n:)) a aPos, pathCost + energy (Full a), updateState state p n)
            | let (Full a) = g M.! p,
              n <- neighborsNoDiags p,
              validMove g (Full a) p n
          ]
        queue' = foldl' (\q st@(g, _, pathCost, _) -> PQ.insert (pathCost + h g) st q) rest nextStates

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

part1 :: Maybe Int
part1 =
  (readGrid maze1 :: Grid Cell)
    & fillDef None
    & organize

part2 :: Maybe Int
part2 =
  (readGrid exx :: Grid Cell)
    & fillDef None
    & organize

debug = False
