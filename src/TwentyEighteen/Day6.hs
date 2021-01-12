module TwentyEighteen.Day6 where

import Data.List (maximumBy, minimumBy, sortOn)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Data.Ord (Down (Down), comparing)
import Util (countMap)

tuplify2 :: [a] -> (a, a)
tuplify2 [x, y] = (x, y)

toCoord :: String -> (Int, Int)
toCoord = tuplify2 . fmap read . splitOn ", "

-- Get the top-left, lower-right bounds of the grid.
-- It's safe to ignore all others because anything extending
-- outside the grid is also infinite.
bounds :: [(Int, Int)] -> ((Int, Int), (Int, Int))
bounds cs =
  ( ( fst $ minimumBy (comparing fst) cs,
      snd $ minimumBy (comparing snd) cs
    ),
    ( fst $ maximumBy (comparing fst) cs,
      snd $ maximumBy (comparing snd) cs
    )
  )

-- Manhattan distance between two points
distance :: (Int, Int) -> (Int, Int) -> Int
distance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

-- Get a map from all grid points to the list of all real points and their distances.
-- Takes a bounds modifier so we can look for infinities.
pointDistances :: [(Int, Int)] -> Int -> M.Map (Int, Int) [((Int, Int), Int)]
pointDistances cs boundsMod = M.fromList distances
  where
    ((minX, minY), (maxX, maxY)) = bounds cs
    allPoints = [(x, y) | x <- [minX - boundsMod .. maxX + boundsMod], y <- [minY - boundsMod .. maxY + boundsMod]]
    distances = [(p, [(c, distance c p) | c <- cs]) | p <- allPoints]

-- Gets the closest point from the given distances. If there is a draw then Nothing.
closestPoint :: [((Int, Int), Int)] -> Maybe (Int, Int)
closestPoint ds = if numMins > 1 then Nothing else Just c
  where
    (c, minDistance) = minimumBy (comparing snd) ds
    numMins = length $ filter (\(_, d) -> d == minDistance) ds

-- Transforms the point-map into a map from point to closest.
pointToClosest :: M.Map (Int, Int) [((Int, Int), Int)] -> M.Map (Int, Int) (Maybe (Int, Int))
pointToClosest = fmap closestPoint

-- Gets the largest finite area from the map.
allAreas :: ((Int, Int), (Int, Int)) -> M.Map (Int, Int) (Maybe (Int, Int)) -> [((Int, Int), Int)]
allAreas bounds = sortOn (Down . snd) . M.toList . countMap . removeEdges bounds . mapMaybe snd . M.toList

-- Get rid of any points that lie on the given bounds.
removeEdges :: ((Int, Int), (Int, Int)) -> [(Int, Int)] -> [(Int, Int)]
removeEdges ((minX, minY), (maxX, maxY)) = filter (\(x, y) -> x /= minX && x /= maxX && y /= minY && y /= maxY)

-- You can still be infinite if you are inside the bounds
-- Horrible hack: try with tight bounds and try with loose bounds, then only take those that don't change.

part1 :: IO Int
part1 = do
  coords <- fmap toCoord . lines <$> readFile "input/2018/6.txt"
  let bs = bounds coords
      pds = pointDistances coords 0
      pds' = pointDistances coords 50
      ptc = pointToClosest pds
      ptc' = pointToClosest pds'
      areas = M.fromList $ allAreas bs ptc
      areas' = M.fromList $ allAreas bs ptc'
   in return $
        snd . head $
          filter (\((_, x, y), _) -> x == y) $
            sortOn (Down . snd) $
              M.toList $
                M.mapKeys (\k -> (k, M.lookup k areas, M.lookup k areas')) areas

part2 :: IO Int
part2 = do
  coords <- fmap toCoord . lines <$> readFile "input/2018/6.txt"
  let pds = pointDistances coords 0
      pointsToSums = filter (\(p, s) -> s < 10000) $ M.toList $ fmap (sum . fmap snd) pds
   in return $ length pointsToSums
