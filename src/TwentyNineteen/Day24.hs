module TwentyNineteen.Day24 where

import Coord (Coord3, neighborsNoDiags)
import Data.List (iterate')
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Grid (Grid, toGrid)
import Util (input)

data Cell = Dead | Alive deriving (Eq, Ord)

fromChar '.' = Dead
fromChar '#' = Alive

step :: Grid Cell -> Grid Cell
step grid =
  M.fromList
    [ ((x, y), c)
      | x <- [0 .. 4],
        y <- [0 .. 4],
        let n = length . filter (== Just Alive) $ M.lookup <$> neighborsNoDiags (x, y) <*> pure grid
            c = case grid M.! (x, y) of
              Dead -> if n `elem` [1, 2] then Alive else Dead
              Alive -> if n == 1 then Alive else Dead
    ]

biodiversity :: Grid Cell -> Int
biodiversity grid =
  sum
    [ 2 ^ (y * 5 + x)
      | x <- [0 .. 4],
        y <- [0 .. 4],
        grid M.! (x, y) == Alive
    ]

firstRepeat :: Grid Cell -> Grid Cell
firstRepeat grid = go grid S.empty
  where
    go grid seen
      | grid `S.member` seen = grid
      | otherwise = go (step grid) (S.insert grid seen)

part1 :: IO Int
part1 = do
  grid <- toGrid fromChar . lines <$> input 2019 24
  return . biodiversity . firstRepeat $ grid

recNeighbors :: Coord3 -> [Coord3]
recNeighbors (x, y, z) = above ++ below ++ left ++ right
  where
    above =
      if y == 0
        then [(2, 1, z - 1)]
        else
          if (x, y) == (2, 3)
            then [(x', 4, z + 1) | x' <- [0 .. 4]]
            else [(x, y - 1, z)]
    below =
      if y == 4
        then [(2, 3, z - 1)]
        else
          if (x, y) == (2, 1)
            then [(x', 0, z + 1) | x' <- [0 .. 4]]
            else [(x, y + 1, z)]
    left =
      if x == 0
        then [(1, 2, z - 1)]
        else
          if (x, y) == (3, 2)
            then [(4, y', z + 1) | y' <- [0 .. 4]]
            else [(x - 1, y, z)]
    right =
      if x == 4
        then [(3, 2, z - 1)]
        else
          if (x, y) == (1, 2)
            then [(0, y', z + 1) | y' <- [0 .. 4]]
            else [(x + 1, y, z)]

stepRec :: Set Coord3 -> Set Coord3
stepRec alive =
  S.fromList
    [ p
      | p <- recNeighbors =<< S.toList alive,
        let n = length $ filter (`S.member` alive) (recNeighbors p),
        (p `S.member` alive && n == 1) || (not (p `S.member` alive) && n `elem` [1, 2])
    ]

part2 :: IO Int
part2 = do
  alive <-
    S.fromList
      . fmap (\(x, y) -> (x, y, 0))
      . M.keys
      . M.filter (== Alive)
      . toGrid fromChar
      . lines
      <$> input 2019 24
  return . S.size $ iterate' stepRec alive !! 200
