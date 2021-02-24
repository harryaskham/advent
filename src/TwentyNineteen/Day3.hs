module TwentyNineteen.Day3 where

import Coord (manhattan)
import Data.List (sortOn)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Util (input)

type Wire = [String]

-- Get all the coordinates that a wire occupies including how long it took to first get there in steps.
wireToCoords :: [String] -> M.Map (Int, Int) Int
wireToCoords = go 0 (0, 0) M.empty
  where
    go :: Int -> (Int, Int) -> M.Map (Int, Int) Int -> [String] -> M.Map (Int, Int) Int
    go _ _ seen [] = seen
    go step (x, y) seen ((d : n) : dirs) = go (step + length segments - 1) (nextX, nextY) insertedSegments dirs
      where
        n' = read n
        (nextX, nextY) = case d of
          'R' -> (x + n', y)
          'L' -> (x - n', y)
          'D' -> (x, y + n')
          'U' -> (x, y - n')
        -- Must sort the segments by distance from current, so that we always follow a continuous path.
        segments = sortOn (manhattan (x, y)) [(x', y') | x' <- allBetween x nextX, y' <- allBetween y nextY]
        -- Convert to a map from segment to the step number on which we land on it.
        segmentSteps = M.fromList $ zip segments [step .. (step + length segments)]
        -- Insert with preference for already seen keys to ensure we keep the smallest distance.
        insertedSegments = M.union seen segmentSteps

-- Get all members between, inclusive. Can provide args in any order.
allBetween :: (Enum a) => a -> a -> [a]
allBetween x y = if fromEnum x <= fromEnum y then [x .. y] else [y .. x]

part1 :: IO Int
part1 = do
  directions <- fmap (splitOn ",") . lines <$> input 2019 3
  let intersections = S.delete (0, 0) $ foldl1 S.intersection (S.fromList . fmap fst . M.toList . wireToCoords <$> directions)
   in return $ minimum (manhattan (0, 0) <$> S.toList intersections)

part2 :: IO (Maybe Int)
part2 = do
  directions <- fmap (splitOn ",") . lines <$> input 2019 3
  let coordMaps = wireToCoords <$> directions
      (coord1, coord2) = (head coordMaps, coordMaps !! 1)
      intersections = S.delete (0, 0) $ foldl1 S.intersection (S.fromList . fmap fst . M.toList . wireToCoords <$> directions)
      intersectionSums = sequenceA $ (\k -> (+) <$> M.lookup k coord1 <*> M.lookup k coord2) <$> S.toList intersections
   in return $ minimum <$> intersectionSums
