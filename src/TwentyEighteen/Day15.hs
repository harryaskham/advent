{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module TwentyEighteen.Day15 where

import Coord hiding (move)
import Data.Bits
import Data.Char
import qualified Data.Foldable as F
import Data.Function
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

-- TODO: here and below, stopping A* as soon as one branch is good doesn't mean the rest are good
-- we need all-shortest-paths instead
{-
getMins :: Ord k => PQ.MinPQueue k a -> [a]
getMins q = let (k, _) = PQ.findMin q in go k q
  where
    go k q
      | PQ.null q = []
      | fst (PQ.findMin q) /= k = []
      | otherwise = let ((_, a), q') = PQ.deleteFindMin q in a : go k q'
-}

-- TODO: Maybe we can do all paths in one swoop disambiguating along the way
-- i.e. don't set a dest, but when we get back to a dest another way, compare paths and see
-- if we should store the new one based on FIRST STEP ONLY this is cheap!

-- Okay this one tiebreaks and gives the paths
-- TODO: Still need to run this once per move
-- It doesn't even complete once, but simple memoization of the
-- blocked / unblocked locations would work for future speedup
-- returns the paths whose first move is in reading order to every position
-- TODO: OMG it also gets it wrong!!! example big is slow, fucks sake
-- we are suffering from the backtracking G again
allPaths :: Grid Cell -> Coord2 -> Map Coord2 [Coord2]
allPaths grid start = go (SQ.singleton ([start], S.empty)) M.empty
  where
    go SQ.Empty paths = paths
    go q@((path@(pos : _), seen) SQ.:<| rest) paths =
      case M.lookup pos paths of
        Nothing -> go (rest SQ.>< next) (M.insert pos (drop 1 . reverse $ path) paths)
        Just oldPath ->
          go
            (rest SQ.>< next)
            (M.insert pos (minimumOn ((,) <$> length <*> (swap . head)) [oldPath, drop 1 . reverse $ path]) paths)
      where
        ns = neighborsNoDiags pos
        canVisit n =
          n `elem` ns -- only consider neighbours
            && M.lookup n grid == Just Empty -- that are empty
            && not (n `S.member` seen) -- that we didn't go to yet
            && (not (n `M.member` paths) || length (paths M.! n) >= length path) -- and stop for longer paths
        nextPositions = filter canVisit $ neighborsNoDiags pos
        next = SQ.fromList [(p, S.insert pos seen) | p <- ((: path) <$> nextPositions)]

shortestBetweenBfs :: Grid Cell -> Coord2 -> Coord2 -> [[Coord2]]
shortestBetweenBfs grid start dest =
  go (SQ.singleton ([start], S.empty)) Nothing []
  where
    go SQ.Empty _ paths = drop 1 . reverse <$> paths
    go ((path@(pos : _), seen) SQ.:<| rest) best paths
      | pos == dest = go rest (Just $ length path) (path : paths)
      | otherwise = traceShow (length rest) $ go (rest SQ.>< next) best paths
      where
        ns = neighborsNoDiags pos
        canVisit n =
          n `elem` ns -- only consider neighbours
            && M.lookup n grid == Just Empty -- that are empty
            && not (n `S.member` seen) -- that we didn't go to yet
            && (isNothing best || Just (length path) < best) -- where it's possible to be a shortest path
        nextPositions = filter canVisit $ neighborsNoDiags pos
        next = SQ.fromList [(p, S.insert pos seen) | p <- ((: path) <$> nextPositions)]

shortestBetweenAStar :: Grid Cell -> Coord2 -> Coord2 -> [[Coord2]]
shortestBetweenAStar grid start dest =
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
         in traceShow (length path, best, start, pos, dest) $
              if pos == dest
                then path : nextRun
                else nextRun

{-
shortestPaths :: Grid Cell -> Coord2 -> Map Coord2 [[Coord2]]
shortestPaths grid start = go (SQ.singleton [start]) M.empty
  where
    go :: Seq ([Coord2]) -> Map Coord2 [[Coord2]] -> Map Coord2 [[Coord2]]
    go SQ.Empty paths = paths
    go (path@(pos : _) SQ.:<| queue) paths =
      go
        (queue SQ.>< next)
        (adjustWithDefault [] ((drop 1 $ reverse path) :) pos paths)
      where
        ns = neighborsNoDiags pos
        canVisit k v =
          k /= start -- don't go back to the start
            && k `elem` ns -- only consider neighbours
            && v == Empty -- that are empty
            && ( isNothing (M.lookup k paths) -- and either we never visited yet
                   || (length . head <$> M.lookup k paths) > Just (length path) -- or it could be another shortest path
               )
        next =
          SQ.fromList
            . fmap (: path)
            . M.keys
            . M.filterWithKey canVisit
            $ grid
-}

nextStep :: Grid Cell -> Coord2 -> Cell -> Maybe Coord2
nextStep grid pos unit
  | null targetPathsDistances = Nothing
  | otherwise = Just selectedStep
  where
    paths = allPaths grid pos
    targets =
      filter
        (\c -> c /= pos && grid M.! c == Empty)
        (nub (neighborsNoDiags =<< (M.keys $ M.filter (enemies unit) grid)))
    targetPathsDistances =
      [ (t, paths M.! t, length $ paths M.! t)
        | t <- targets,
          t `M.member` paths
      ]
    validPaths = fmap snd3 . head . groupOn thd3 . sortOn thd3 $ targetPathsDistances
    validSteps = head <$> validPaths
    selectedStep = head $ sortOn swap validSteps

{-
    pathsToTargets = shortestBetweenBfs grid pos <$> targets
    targetPathsDistance :: [(Coord2, [[Coord2]], Int)]
    targetPathsDistance =
      [ (t, paths, length (head paths))
        | (t, paths) <- zip targets pathsToTargets,
          not (null paths)
      ]
    closestTriples :: [(Coord2, [[Coord2]], Int)]
    closestTriples =
      head
        . groupOn fst3
        . sortOn (swap . fst3)
        . head
        . groupOn thd3
        . sortOn thd3
        $ targetPathsDistance
    targetPaths = snd3 =<< closestTriples
    steps = head <$> targetPaths
    selectedStep = head $ sortOn swap steps
-}

move :: Coord2 -> Coord2 -> Grid Cell -> Grid Cell
move source dest grid =
  M.insert dest (grid M.! source) . M.insert source Empty $ grid

moveToTarget :: Grid Cell -> Coord2 -> Cell -> Maybe (Grid Cell, Coord2)
moveToTarget grid pos unit =
  case nextStep grid pos unit of
    Nothing -> Nothing
    Just dest -> Just (move pos dest grid, dest)

canAttack :: Grid Cell -> Coord2 -> Cell -> Bool
canAttack grid pos unit = not . null $ attackTargets grid pos unit

attackTargets :: Grid Cell -> Coord2 -> Cell -> [(Coord2, Cell)]
attackTargets grid pos unit = sortOn (\(_, u) -> getHp u) ts
  where
    ns = neighborsNoDiags pos
    ts = M.toList $ M.filterWithKey (\k v -> k `elem` ns && enemies unit v) grid

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
  case attackTargets grid pos unit of
    [] -> grid
    ((tPos, tUnit) : _) -> performAttack grid tPos tUnit (getAtk unit)

data EndTurn = Running (Grid Cell) | GameOver (Grid Cell)

cellTurn :: Grid Cell -> (Coord2, Cell) -> EndTurn
cellTurn grid (pos, unit)
  | gameOver grid = GameOver grid
  | grid M.! pos == Empty = Running grid
  | canAttack grid pos unit = traceShow (unit, pos) $ traceShow "attack" Running $ attack grid pos unit
  | otherwise = traceShow (unit, pos) $
    case moveToTarget grid pos unit of
      Nothing -> traceShow "can't move" $ Running grid
      Just (grid', pos') ->
        traceShow "moved" $
          if canAttack grid' pos' unit
            then traceShow "attack" $ Running $ attack grid' pos' unit
            else traceShow "no attack" $ Running grid'

gameTurn :: Grid Cell -> EndTurn
gameTurn grid = runTurns grid sortedCells
  where
    sortedCells = filter (\(_, c) -> isElf c || isGoblin c) $ sortOn (\((x, y), _) -> (y, x)) $ M.toList grid
    runTurns grid [] = Running grid
    runTurns grid (c : cs) =
      case cellTurn grid c of
        GameOver grid' -> traceStrLn (pretty grid') $ GameOver grid'
        Running grid' -> traceStrLn (pretty grid') $ runTurns grid' cs

gameOver :: Grid Cell -> Bool
gameOver grid = noElves || noGoblins
  where
    noElves = M.size (M.filter (isElf) grid) == 0
    noGoblins = M.size (M.filter (isGoblin) grid) == 0

runGame :: Int -> Grid Cell -> (Int, Grid Cell)
runGame n grid = traceShow n $
  case gameTurn grid of
    Running grid' -> runGame (n + 1) grid'
    GameOver grid' -> (n, grid')

part1 :: IO Int
part1 = do
  (n, grid) <- runGame 0 . toGrid fromChar . lines <$> exampleInput 2018 15
  let totalHp = sum (getHp <$> grid)
  print (n, totalHp)
  return $ n * totalHp
