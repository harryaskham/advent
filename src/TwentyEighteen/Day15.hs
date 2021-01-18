{-# LANGUAGE TupleSections #-}

module TwentyEighteen.Day15 where

import Coord hiding (move)
import Data.Bits
import Data.Char
import qualified Data.Foldable as F
import Data.Function
import Data.IORef
import Data.List
import Data.List.Extra
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.PQueue.Prio.Min as PQ
import Data.Sequence (Seq)
import qualified Data.Sequence as SQ
import Data.Set (Set)
import qualified Data.Set as S
import Data.Tuple.Extra
import Data.Vector (Vector)
import qualified Data.Vector as V
import Debug.Trace
import Grid
import System.IO.Unsafe
import Text.ParserCombinators.Parsec
import Text.RawString.QQ (r)
import Util

data Cell
  = Wall
  | Empty
  | Elf Int Int
  | Goblin Int Int
  deriving (Eq, Ord)

instance Show Cell where
  show Wall = "#"
  show Empty = "."
  show (Elf _ _) = "E"
  show (Goblin _ _) = "G"

fromChar :: Char -> Cell
fromChar '#' = Wall
fromChar '.' = Empty
fromChar 'G' = Goblin 200 3
fromChar 'E' = Elf 200 3

isGoblin :: Cell -> Bool
isGoblin (Goblin _ _) = True
isGoblin _ = False

isElf :: Cell -> Bool
isElf (Elf _ _) = True
isElf _ = False

getHp :: Cell -> Int
getHp (Elf hp _) = hp
getHp (Goblin hp _) = hp
getHp _ = 0

getAtk :: Cell -> Int
getAtk (Elf _ atk) = atk
getAtk (Goblin _ atk) = atk

enemies :: Cell -> Cell -> Bool
enemies (Elf _ _) (Goblin _ _) = True
enemies (Goblin _ _) (Elf _ _) = True
enemies _ _ = False

bestPath :: [Coord2] -> [Coord2] -> [Coord2]
bestPath p1 p2 = minimumOn ((,) <$> length <*> (swap . head)) [p1, p2]

fixPath :: [Coord2] -> [Coord2]
fixPath = drop 1 . reverse

allPathsBfs :: Grid Cell -> Coord2 -> [Coord2] -> Map Coord2 [Coord2]
allPathsBfs grid start targets = go (SQ.singleton [start]) M.empty
  where
    go SQ.Empty paths = paths
    go (path@(pos : _) SQ.:<| rest) paths
      -- at this point, if we are already on long paths, we can stop
      | not (null targetPaths) && length fixedPath > minimum (length <$> targetPaths) = paths
      -- if we already have a path to this point then keep only the best one
      | pos `M.member` paths = go rest (M.insert pos (bestPath (paths M.! pos) fixedPath) paths)
      -- otherwise track the path for the first time
      | otherwise = go (rest SQ.>< next) (M.insert pos fixedPath paths)
      where
        fixedPath = fixPath path
        targetPaths = catMaybes $ M.lookup <$> targets <*> pure paths
        ns = neighborsNoDiags pos
        canVisit n =
          n `elem` ns
            && M.lookup n grid == Just Empty
        -- && not (n `M.member` paths)
        nextPositions = sortOn swap $ filter canVisit ns
        next = SQ.fromList ((: path) <$> nextPositions)

allPathsDfs :: Grid Cell -> Coord2 -> Map Coord2 [Coord2]
allPathsDfs grid start = unsafePerformIO $ do
  pathsRef <- newIORef M.empty
  go pathsRef [start]
  readIORef pathsRef
  where
    go :: IORef (Map Coord2 [Coord2]) -> [Coord2] -> IO ()
    go pathsRef path@(pos : _) = do
      paths <- readIORef pathsRef
      let fixedPath = fixPath path
          oldPath = M.lookup pos paths
          newPaths =
            case oldPath of
              Nothing -> M.insert pos fixedPath paths
              Just op -> M.insert pos (bestPath op fixedPath) paths
          ns = neighborsNoDiags pos
          canVisit n =
            n `elem` ns
              && M.lookup n grid == Just Empty
              && (not (n `M.member` paths) || (length <$> oldPath) > Just (length fixedPath))
          nextPositions = sortOn swap $ filter canVisit ns
      modifyIORef' pathsRef (const newPaths)
      mapM_ (go pathsRef) ((: path) <$> nextPositions)

shortestBetweenAStarAllPaths :: Grid Cell -> Coord2 -> Coord2 -> [[Coord2]]
shortestBetweenAStarAllPaths grid start dest =
  drop 1 . reverse <$> go (PQ.singleton (h start) ([start], S.singleton start)) Nothing
  where
    h pos = manhattan pos dest
    go :: PQ.MinPQueue Int ([Coord2], Set Coord2) -> Maybe Int -> [[Coord2]]
    go queue best
      | PQ.null queue = []
      | otherwise =
        let ((_, (path@(pos : _), seen)), queue') = PQ.deleteFindMin queue
            ns = neighborsNoDiags pos
            canVisit n =
              n `elem` ns -- only consider neighbours
                && M.lookup n grid == Just Empty -- that are empty
                && not (n `S.member` seen) -- that we didn't go to yet
                && (isNothing best || Just (length path + h n) < best) -- where it's possible to be a shortest path
            nextPositions = filter canVisit $ neighborsNoDiags pos
            nextStates = (\p -> (length path + 1 + h pos, p : path, S.insert p seen)) <$> nextPositions
            nextBest = if pos == dest then Just (length path) else best
            nextRun = go (foldl' (\q (k, paths, seen) -> PQ.insert k (paths, seen) q) queue' nextStates) nextBest
         in -- traceShow (length path, best, start, pos, dest) $
            if pos == dest
              then path : nextRun
              else nextRun

-- TODO: Do an early-terminating all-targets A* starting with the targets closest manhattanwise
-- TODO: That will place caps on the other A* searches
-- TODO: Could also consider an A* with a cost function that naturally takes it towards the closest most appropriate target
-- TODO: So zero when we're on top-left

-- TODO: e.g. terminate after N steps once we've found the good one
shortestBetweenAStarMany :: Grid Cell -> Coord2 -> [Coord2] -> Map Coord2 [Coord2]
shortestBetweenAStarMany grid start targets =
  go (sortOn (manhattan start) targets) Nothing M.empty
  where
    go [] _ paths = paths
    go (t : ts) best paths =
      let path = shortestBetweenAStar grid start t best
       in go ts (if Just (length path) < best then Just (length path) else best) (M.insert t path paths)

shortestBetweenAStar :: Grid Cell -> Coord2 -> Coord2 -> Maybe Int -> [Coord2]
shortestBetweenAStar grid start dest stopAfter =
  -- traceShow (start, dest) $
  drop 1 . reverse $ go (PQ.singleton (h start) ([start], S.singleton start))
  where
    h pos = manhattan pos dest
    go :: PQ.MinPQueue Int ([Coord2], Set Coord2) -> [Coord2]
    go queue
      | PQ.null queue = []
      | otherwise =
        let ((_, (path@(pos : _), seen)), queue') = PQ.deleteFindMin queue
            ns = neighborsNoDiags pos
            canVisit n =
              n `elem` ns -- only consider neighbours
                && M.lookup n grid == Just Empty -- that are empty
                && not (n `S.member` seen) -- that we didn't go to yet
                && (isNothing stopAfter || Just (length path + 1) <= stopAfter)
            -- explore in reading order
            nextPositions = sortOn swap $ filter canVisit $ neighborsNoDiags pos
            nextStates = (\p -> (length path + 1 + h pos, p : path, S.insert p seen)) <$> nextPositions
            nextRun = go (foldl' (\q (k, paths, seen) -> PQ.insert k (paths, seen) q) queue' nextStates)
         in --traceShow (PQ.size queue, length path) $
            if pos == dest then path else nextRun

reachable :: Grid Cell -> Coord2 -> Set Coord2
reachable grid start = go (SQ.singleton start) S.empty
  where
    go SQ.Empty seen = seen
    go (pos SQ.:<| queue) seen = go (queue SQ.>< next) (S.insert pos seen)
      where
        next =
          SQ.fromList
            [ p
              | p <- neighborsNoDiags pos,
                M.lookup p grid == Just Empty,
                not $ p `S.member` seen
            ]

-- Faster reachability check to see if any enemy is reachable
enemyReachable :: Grid Cell -> Coord2 -> Cell -> Bool
enemyReachable grid pos unit = go (SQ.singleton pos) S.empty
  where
    go SQ.Empty _ = False
    go (pos SQ.:<| queue) seen
      | enemyFound = True
      | pos `S.member` seen = go queue seen
      | otherwise = go (queue SQ.>< next) (S.insert pos seen)
      where
        ns = neighborsNoDiags pos
        enemyFound = any (enemies unit) (catMaybes $ M.lookup <$> ns <*> pure grid)
        next =
          SQ.fromList
            [ p
              | p <- ns,
                M.lookup p grid == Just Empty,
                not $ p `S.member` seen
            ]

nextStepDfs :: Grid Cell -> Coord2 -> Cell -> Maybe Coord2
nextStepDfs grid pos unit
  | null targetPathDistance = Nothing
  | otherwise = Just selectedStep
  where
    paths = allPathsDfs grid pos
    targetPathDistance =
      [ (t, paths M.! t, length (paths M.! t))
        | t <- (nub (neighborsNoDiags =<< (M.keys $ M.filter (enemies unit) grid))),
          M.lookup t grid == Just Empty,
          t `M.member` paths
      ]
    selectedStep =
      head
        . snd3
        . head
        . sortOn (swap . fst3)
        . head
        . groupOn thd3
        . sortOn thd3
        $ targetPathDistance

nextStepBfs :: Grid Cell -> Coord2 -> Cell -> Maybe Coord2
nextStepBfs grid pos unit
  | null targetPathDistance = Nothing
  | otherwise = Just selectedStep
  where
    targets =
      [ t
        | t <- (nub (neighborsNoDiags =<< (M.keys $ M.filter (enemies unit) grid))),
          M.lookup t grid == Just Empty
      ]
    paths = allPathsBfs grid pos targets
    targetPathDistance =
      [ (t, paths M.! t, length (paths M.! t))
        | t <- targets,
          t `M.member` paths
      ]
    selectedStep =
      head
        . snd3
        . head
        . sortOn (swap . fst3)
        . head
        . groupOn thd3
        . sortOn thd3
        $ targetPathDistance

move :: Coord2 -> Coord2 -> Grid Cell -> Grid Cell
move source dest grid =
  M.insert dest (grid M.! source) . M.insert source Empty $ grid

moveToTarget :: Grid Cell -> Coord2 -> Cell -> Maybe (Grid Cell, Coord2)
moveToTarget grid pos unit =
  case nextStepBfs grid pos unit of
    Nothing -> Nothing
    Just dest -> Just (move pos dest grid, dest)

canAttack :: Grid Cell -> Coord2 -> Cell -> Bool
canAttack grid pos unit = isJust $ attackTarget grid pos unit

attackTarget :: Grid Cell -> Coord2 -> Cell -> Maybe (Coord2, Cell)
attackTarget grid pos unit
  | null targets = Nothing
  | otherwise = Just $ head $ sortOn (\(c, u) -> (getHp u, swap c)) targets
  where
    ns = neighborsNoDiags pos
    targets = M.toList $ M.filterWithKey (\k v -> k `elem` ns && enemies unit v) grid

performAttack :: Grid Cell -> Coord2 -> Cell -> Int -> Grid Cell
performAttack grid tPos tUnit atk
  | getHp tUnit <= atk = kill grid tPos
  | otherwise = M.insert tPos (withDamage tUnit atk) grid

withDamage :: Cell -> Int -> Cell
withDamage (Goblin hp atk) dmg = Goblin (hp - dmg) atk
withDamage (Elf hp atk) dmg = Elf (hp - dmg) atk

kill :: Grid Cell -> Coord2 -> Grid Cell
kill grid pos = M.insert pos Empty grid

attack :: Grid Cell -> Coord2 -> Cell -> Grid Cell
attack grid pos unit =
  case attackTarget grid pos unit of
    Nothing -> grid
    Just (tPos, tUnit) -> traceShow (getAtk unit) $ performAttack grid tPos tUnit (getAtk unit)

data EndTurn = Running (Grid Cell) | GameOver (Grid Cell)

cellTurn :: Grid Cell -> (Coord2, Cell) -> EndTurn
cellTurn grid (pos, unit)
  | gameOver grid = GameOver grid
  | grid M.! pos == Empty = Running grid
  | canAttack grid pos unit =
    traceShow (unit, pos) $
      traceShow "attack" $
        Running $ attack grid pos unit
  | otherwise =
    traceShow (unit, pos) $
      case moveToTarget grid pos unit of
        Nothing ->
          traceShow "can't move" $
            Running grid
        Just (grid', pos') ->
          traceShow "moved" $
            if canAttack grid' pos' unit
              then
                traceShow "attack" $
                  Running $ attack grid' pos' unit
              else
                traceShow "no attack" $
                  Running grid'

gameTurn :: Grid Cell -> EndTurn
gameTurn grid = runTurns grid sortedCells
  where
    sortedCells = filter (\(_, c) -> isElf c || isGoblin c) $ sortOn (\((x, y), _) -> (y, x)) $ M.toList grid
    runTurns grid [] = Running grid
    runTurns grid (c : cs) =
      case cellTurn grid c of
        GameOver grid' ->
          traceStrLn (pretty grid') $
            GameOver grid'
        Running grid' ->
          traceStrLn (pretty grid') $
            runTurns grid' cs

gameOver :: Grid Cell -> Bool
gameOver grid = noElves || noGoblins
  where
    noElves = M.size (M.filter (isElf) grid) == 0
    noGoblins = M.size (M.filter (isGoblin) grid) == 0

runGame :: Int -> Grid Cell -> (Int, Grid Cell)
runGame n grid =
  traceShow n $
    case gameTurn grid of
      Running grid' -> runGame (n + 1) grid'
      GameOver grid' -> (n, grid')

solve :: Grid Cell -> Int
solve grid = n * totalHp
  where
    (n, grid') = runGame 0 grid
    totalHp = sum (getHp <$> grid')

part1 :: IO Int
part1 = solve . toGrid fromChar . lines <$> input 2018 15

setAtk :: Int -> Cell -> Cell
setAtk atk (Elf hp _) = Elf hp atk
setAtk _ c = c

numElves :: Grid Cell -> Int
numElves grid = M.size (M.filter (isElf) grid)

runGameUnlessDeath :: Int -> Int -> Grid Cell -> Maybe (Int, Grid Cell)
runGameUnlessDeath n beforeElves grid
  | numElves grid < beforeElves = Nothing
  | otherwise =
    case gameTurn grid of
      Running grid' -> runGameUnlessDeath (n + 1) beforeElves grid'
      GameOver grid' -> Just (n, grid')

solveUntilAllElvesSurvive :: [Grid Cell] -> Int
solveUntilAllElvesSurvive (grid : grids) =
  case runGameUnlessDeath 0 (numElves grid) grid of
    Nothing -> solveUntilAllElvesSurvive grids
    Just (n, grid') -> n * sum (getHp <$> grid')

part2 :: IO Int
part2 = do
  grid <- toGrid fromChar . lines <$> input 2018 15
  let grids = [setAtk atk <$> grid | atk <- [4 ..]]
  return $ solveUntilAllElvesSurvive grids
