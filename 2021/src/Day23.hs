{-# LANGUAGE QuasiQuotes #-}

-- TODO:
-- Remove grid from pqueue, only have walls and member positions
-- bfs-moves; for each thing; bfs to find possible destinations, then each is a "movement" and gets added to the queue

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

rooms :: Map Coord2 (Amphipod, [Coord2])
rooms = M.fromList [(d, (a, ds)) | a <- enumerate, let ds = destinations a, d <- ds]

-- Can this amphipod move to this position?
-- Not the hallway condition, only the room one
validMove :: Grid Cell -> Cell -> Coord2 -> Coord2 -> Bool
validMove g a p n
  | (g M.! n) /= Empty = False
  | otherwise =
    case M.lookup n rooms of
      Nothing -> True
      Just (a', ps) ->
        p `elem` ps -- if we're already in the room, doesn't matter if it's ours, we can move
          || a == Full a' -- then we're hallway, so if it's a room, it's the right room, and also it's got the right ones in it
            && length [a'' | a'' <- (g M.!) <$> ps, a'' `elem` [Empty, a]] == length [p | p <- ps, g M.! p /= Wall]

makeMove :: (Coord2, Coord2) -> Grid Cell -> Grid Cell
makeMove (a, b) g = M.insert a Empty . M.insert b (g M.! a) $ g

illegallyOccupied :: Grid Cell -> [Coord2]
illegallyOccupied g =
  case [p | p <- illegalStops, (g M.! p) /= Empty] of
    [] -> []
    [p] -> [p]
    _ -> error "multiple illegals"

organizeDfs :: Grid Cell -> IO (Maybe Int)
organizeDfs g = do
  bestRef <- newIORef Nothing
  aPosCostsRef <- newIORef M.empty
  let go :: Grid Cell -> Map Amphipod [(Int, Int)] -> Int -> Maybe (Coord2, Coord2) -> Set (Map Amphipod [Coord2]) -> IO (Maybe Int)
      go g aPos pathCost state seen
        | organized g aPos = do
          print "Found an organized version"
          print pathCost
          best <- readIORef bestRef
          case best of
            Nothing -> do
              modifyIORef' bestRef (const $ Just pathCost)
              return (Just pathCost)
            Just b -> do
              let newBest = min b pathCost
              modifyIORef' bestRef (const $ Just newBest)
              return (Just newBest)
        | otherwise = do
          best <- readIORef bestRef
          print ("best", best)
          print pathCost
          aPosCosts <- readIORef aPosCostsRef
          let runOn = do
                case M.lookup aPos aPosCosts of
                  Just cost -> do
                    print "found in cache, terminating"
                    return $ Just (pathCost + cost)
                  Nothing -> do
                    let next = nextStates g aPos pathCost state
                        seen' = S.insert aPos seen
                    aPosCosts <- sequence [(aPos,) <$> go g aPos pathCost state seen' | (g, aPos, pathCost, state) <- next, not (aPos `S.member` seen)]
                    let costs = catMaybes $ snd <$> aPosCosts
                        aPosCostMap = M.fromList (second (\(Just a) -> a) <$> (filter (isJust . snd) aPosCosts))
                    case costs of
                      [] -> return Nothing
                      _ -> do
                        modifyIORef' aPosCostsRef (M.unionWith min aPosCostMap)
                        return $ Just $ L.minimum costs
          case best of
            Just b ->
              if b < pathCost
                then do
                  print "early terminating, went past best"
                  print pathCost
                  return (Just b)
                else runOn
            Nothing -> runOn
  go g (positions g) 0 Nothing S.empty

-- keep track of whether we just moved the same one, or a new one
updateState :: Maybe (Coord2, Coord2) -> Coord2 -> Coord2 -> Maybe (Coord2, Coord2)
updateState Nothing p n = Just (n, p)
updateState (Just (lastPos, origin)) p n
  | p == lastPos = Just (n, origin)
  | otherwise = Just (n, p)

allMoves :: Grid Cell -> Map Amphipod [Coord2] -> Int -> Maybe (Coord2, Coord2) -> [(Grid Cell, Map Amphipod [Coord2], Int, Maybe (Coord2, Coord2))]
allMoves g aPos pathCost state = movesFor =<< [Amber, Bronze, Copper, Desert]
  where
    movesFor a =
      [ (makeMove (p, n) g, M.adjust (L.delete p . (n :)) a aPos, pathCost + energy (Full a), updateState state p n)
        | p <- aPos M.! a,
          n <- neighborsNoDiags p,
          validMove g (Full a) p n
      ]

movingOne :: Map (Int, Int) Cell -> Map Amphipod [Coord2] -> Int -> Maybe (Coord2, Coord2) -> (Int, Int) -> [(Grid Cell, Map Amphipod [(Int, Int)], Int, Maybe (Coord2, Coord2))]
movingOne g aPos pathCost state p =
  [ (makeMove (p, n) g, M.adjust (L.delete p . (n :)) a aPos, pathCost + energy (Full a), updateState state p n)
    | let (Full a) = g M.! p,
      n <- neighborsNoDiags p,
      validMove g (Full a) p n
  ]

nextStates :: Grid Cell -> Map Amphipod [Coord2] -> Int -> Maybe (Coord2, Coord2) -> [(Grid Cell, Map Amphipod [(Int, Int)], Int, Maybe (Coord2, Coord2))]
nextStates g aPos pathCost state =
  let ios = illegallyOccupied g
   in if not (null ios)
        then -- always move illegals first

        -- traceWhen debug (traceShow ("illegal occupation, must move:", illegallyOccupied, state)) $
          movingOne g aPos pathCost state (L.head ios)
        else -- TODO: if we're moving an apod that started in the hallway, wemust keep moving that apod
        case state of
          Nothing ->
            -- traceWhen debug (traceShow ("no state, so any move allowed")) $
            allMoves g aPos pathCost state
          Just (lastPos, origin) ->
            case M.lookup origin rooms of
              Just _ ->
                -- traceWhen debug (traceShow ("current mover started in a room, any move allowed")) $
                allMoves g aPos pathCost state -- we started in a room so free to stop anywhere
              Nothing ->
                -- traceWhen debug (traceShow ("current mover started the hall")) $
                -- we started in the hallway so we have to stop in a room
                case M.lookup lastPos rooms of
                  Just _ ->
                    -- traceWhen debug (traceShow ("current hallway mover made it to a room so that's okay")) $
                    allMoves g aPos pathCost state -- our last-moved landed in a room so that's fine
                  Nothing ->
                    -- traceWhen debug (traceShow ("current hallway mover still in hall so need to move him")) $
                    movingOne g aPos pathCost state lastPos -- our last moved is going hallway to hallway and needs to move
                    -- If an apod just moved, then we're still "in its move" and it can stop in the hallway
                    -- We should store whether apod is in its move and whetehr it started from a hallway

organize :: Grid Cell -> Maybe Int
organize g' = go (PQ.singleton (0, 0) (g', positions g', 0, Nothing)) S.empty
  where
    go queue seen
      | PQ.null queue = Nothing
      | organized g aPos = Just pathCost
      | aPos `S.member` seen = go rest seen
      | otherwise =
        traceWhen (cost `mod` 1000 == 0) (traceShow (pathCost, cost)) $
          traceWhen
            debug
            ( pauseId $
                traceShow (state, "pathCost", pathCost, "heuristic", h g aPos, "pq cost", cost) $
                  traceTextLn (pretty g)
            )
            $ go queue' seen'
      where
        (((cost, _), (g, aPos, pathCost, state)), rest) = PQ.deleteFindMin queue
        seen' = S.insert aPos seen
        -- Because we don't penalise being above an empty, at the very least every empty below the first requires additional moves
        -- EEE = 6, EEF = 3, EFE = 4, FEE = 5, EFF = 1, FEF = 2
        binCosts g a ds =
          let empties = (`elem` [Full a, Wall]) . (g M.!) <$> ds
           in case empties of
                [False, False, False] -> 6
                [False, False, True] -> 3
                [False, True, False] -> 4
                [False, True, True] -> 1
                [True, False, False] -> 5
                [True, False, True] -> 2
                [True, True, False] -> 3
                [True, True, True] -> 0
        --roomCost g = sum [energy (Full a) | a <- enumerate, let ds = destinations a, d@(dx, dy) <- ds, g M.! d /= Full a, dy > 2]
        roomCost g = sum [energy (Full a) * binCosts g a (drop 1 ds) | a <- enumerate, let ds = destinations a]
        -- heuristic:
        -- ignore those already in their places
        -- if you are not in your place:
        -- minimum distance to an empty space
        -- TODO: in hole but there's space below
        minDistanceToDest g p@(x, y) a
          -- if we're in the hole, fine
          | x == dx && y > 1 = 0
          -- if we're above our hole, shortest distance to an empty
          | x == dx && y == 1 = 1 -- L.minimum [manhattan p d | d <- ds, g M.! d /= (Full a)]
          -- if we're in another bucket, get to the top then distance to an empty. also works if we're at the top
          | x /= dx = y - 1 + L.minimum [manhattan p d | d <- ds, g M.! d /= Full a]
          | otherwise = error "wat"
          where
            ds@((dx, _) : _) = destinations a
        -- TODO: Better heuristic might help a lot
        minDistanceToDest' _ p a = L.minimum (manhattan p <$> destinations a)
        --h g aPos = roomCost g + sum [energy (Full a) * minDistanceToDest' p a | a <- enumerate, p <- aPos M.! a]
        --h g aPos = roomCost g + sum [energy (Full a) * minDistanceToDest g p a | a <- enumerate, p <- aPos M.! a]
        --h g aPos = sum [energy (Full a) * minDistanceToDest' p a | a <- enumerate, p <- aPos M.! a]
        --h g aPos = sum [energy (Full a) * minDistanceToDest p a | a <- enumerate, p <- aPos M.! a]
        h g aPos = sum [energy (Full a) * minDistanceToDest' g p a | a <- enumerate, p <- aPos M.! a]
        queue' =
          foldl'
            ( \q st@(g, aPos, pathCost, _) ->
                --traceTextLn (pretty g) $
                -- traceShow ("new state",pathCost,h g aPos,pathCost + h g aPos) $
                PQ.insert (pathCost + h g aPos, pathCost) st q
            )
            rest
            (nextStates g aPos pathCost state)

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

exx8 =
  [s|
|]

exx9 =
  [s|
|]

part1 =
  (readGrid maze1 :: Grid Cell)
    & fillDef None
    & organize

-- & organizeDfs

part2 =
  (readGrid maze2 :: Grid Cell)
    & fillDef None
    & organize

part2' =
  (readGrid exx2 :: Grid Cell)
    & fillDef None
    -- & organize
    & organizeDfs

debug = False
