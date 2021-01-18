module TwentyEighteen.Day15 where

import Coord (Coord2, neighborsNoDiags)
import Data.List (nub, sortOn)
import Data.List.Extra (groupOn, minimumOn)
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, isJust)
import qualified Data.Sequence as SQ
import Data.Tuple.Extra (fst3, snd3, swap, thd3)
import Grid (Grid, pretty, toGrid)
import Util (input, traceStrLn)

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

allPaths :: Grid Cell -> Coord2 -> [Coord2] -> Map Coord2 [Coord2]
allPaths grid start targets = go (SQ.singleton [start]) M.empty
  where
    go SQ.Empty paths = paths
    go (path@(pos : _) SQ.:<| rest) paths
      | not (null targetPaths) && length fixedPath > minimum (length <$> targetPaths) = paths
      | pos `M.member` paths = go rest (M.insert pos (bestPath (paths M.! pos) fixedPath) paths)
      | otherwise = go (rest SQ.>< next) (M.insert pos fixedPath paths)
      where
        fixedPath = fixPath path
        targetPaths = catMaybes $ M.lookup <$> targets <*> pure paths
        ns = neighborsNoDiags pos
        canVisit n =
          n `elem` ns
            && M.lookup n grid == Just Empty
        nextPositions = sortOn swap $ filter canVisit ns
        next = SQ.fromList ((: path) <$> nextPositions)

nextStep :: Grid Cell -> Coord2 -> Cell -> Maybe Coord2
nextStep grid pos unit
  | null targetPathDistance = Nothing
  | otherwise = Just selectedStep
  where
    targets =
      [ t
        | t <- nub (neighborsNoDiags =<< M.keys (M.filter (enemies unit) grid)),
          M.lookup t grid == Just Empty
      ]
    paths = allPaths grid pos targets
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
  case nextStep grid pos unit of
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
    Just (tPos, tUnit) -> performAttack grid tPos tUnit (getAtk unit)

data EndTurn = Running (Grid Cell) | GameOver (Grid Cell)

cellTurn :: Grid Cell -> (Coord2, Cell) -> EndTurn
cellTurn grid (pos, unit)
  | gameOver grid = GameOver grid
  | grid M.! pos == Empty = Running grid
  | canAttack grid pos unit =
    Running $ attack grid pos unit
  | otherwise =
    case moveToTarget grid pos unit of
      Nothing ->
        Running grid
      Just (grid', pos') ->
        if canAttack grid' pos' unit
          then Running $ attack grid' pos' unit
          else Running grid'

gameTurn :: Grid Cell -> EndTurn
gameTurn grid = runTurns grid sortedCells
  where
    sortedCells =
      filter
        (\(_, c) -> isElf c || isGoblin c)
        (sortOn (swap . fst) (M.toList grid))
    runTurns grid [] = Running grid
    runTurns grid (c : cs) =
      case cellTurn grid c of
        GameOver grid' ->
          traceStrLn (pretty grid') $ GameOver grid'
        Running grid' ->
          traceStrLn (pretty grid') $ runTurns grid' cs

numElves :: Grid Cell -> Int
numElves grid = M.size (M.filter isElf grid)

numGoblins :: Grid Cell -> Int
numGoblins grid = M.size (M.filter isGoblin grid)

gameOver :: Grid Cell -> Bool
gameOver grid = numElves grid == 0 || numGoblins grid == 0

runGame :: Int -> Grid Cell -> (Int, Grid Cell)
runGame n grid =
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
