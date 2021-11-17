module TwentyNineteen.Day10 where

import Data.Foldable (maximumBy)
import Data.List (nub, sortOn)
import Data.Maybe (catMaybes)
import Data.Ord (Down (Down), comparing)
import Data.Ratio (denominator, numerator, (%))
import qualified Data.Vector as V
import Util (input)

data Asteroid = Asteroid | NoAsteroid deriving (Eq)

type Grid = V.Vector (V.Vector Asteroid)

toAsteroid :: Char -> Asteroid
toAsteroid '#' = Asteroid
toAsteroid '.' = NoAsteroid

instance Show Asteroid where
  show Asteroid = "#"
  show NoAsteroid = "."

-- Approach:
-- All combinations of increments from (-width,-height) to (+width,+height)
-- Reduce these down to smallest integers of same ratio, remove duplicates
-- From a given asteroid, follow increments and count intersections
rayIncrements :: Grid -> [(Int, Int)]
rayIncrements grid = nub $ simplifyRatio <$> allCoords
  where
    width = V.length $ grid V.! 0
    height = V.length grid
    allCoords =
      [ (x, y)
        | x <- [1 - width .. width - 1],
          y <- [1 - height .. height - 1],
          (x, y) /= (0, 0)
      ]

-- Simplify e.g. (10,5) to (2,1)
simplifyRatio :: (Int, Int) -> (Int, Int)
simplifyRatio (0, 0) = (0, 0)
simplifyRatio (0, y) = (0, y `div` abs y)
simplifyRatio (x, 0) = (x `div` abs x, 0)
simplifyRatio (x, y) = preserveSign (x, y) $ (,) <$> numerator <*> denominator $ x % y

-- Ensure the ratio doesn't override signs
preserveSign (x', y') (x, y) =
  ( if x' == x then x else negate x,
    if y' == y then y else negate y
  )

-- Count the number of rays from this position that have intersections on the grid.
numIntersections :: Grid -> (Int, Int) -> Int
numIntersections grid (x, y) =
  length . catMaybes $ intersectAsteroid grid (x, y) <$> rayIncrements grid

-- Follows the ray from the given asteroid and returns the first hit, if any.
intersectAsteroid :: Grid -> (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
intersectAsteroid grid (x, y) (incX, incY) =
  if x + incX >= 0 && x + incX < width && y + incY >= 0 && y + incY < height
    then
      if grid V.! (y + incY) V.! (x + incX) == Asteroid
        then Just (x + incX, y + incY)
        else intersectAsteroid grid (x + incX, y + incY) (incX, incY)
    else Nothing
  where
    width = V.length $ grid V.! 0
    height = V.length grid

asteroidLocations :: Grid -> [(Int, Int)]
asteroidLocations grid =
  [ (x, y)
    | x <- [0 .. width -1],
      y <- [0 .. height -1],
      grid V.! y V.! x == Asteroid
  ]
  where
    width = V.length $ grid V.! 0
    height = V.length grid

-- For part 2:
-- Need to sort offsets by the angle they make
-- Then cycle through, intersecting and sploding the asteroid
-- Once we're at 200 don't splode it and instead return the coord

-- Gets the offsets sorted by angle from the upward vertical.
radialOffsets :: Grid -> [(Int, Int)]
radialOffsets grid = dropWhile (/= (0, -1)) $ cycle sortedOffsets
  where
    sortedOffsets =
      sortOn
        (Down . (\(x, y) -> atan2 (fromIntegral x) (fromIntegral y)))
        (rayIncrements grid)

explode :: (Int, Int) -> Grid -> Grid
explode (x, y) grid = grid V.// [(y, (grid V.! y) V.// [(x, NoAsteroid)])]

-- Keep exploding asteroids radially until we exploded N.
-- Return all exploded asteroids in order.
explodeUntil :: (Int, Int) -> Int -> Grid -> [(Int, Int)]
explodeUntil base n grid = reverse $ go n grid (radialOffsets grid) []
  where
    go 0 _ _ acc = acc
    go n grid (ray : rays) acc = case intersectAsteroid grid base ray of
      Just hit -> go (n -1) (explode hit grid) rays (hit : acc)
      Nothing -> go n grid rays acc

part12 :: IO (Int, Int)
part12 = do
  ls <- lines <$> input 2019 10
  let grid = V.fromList $ V.fromList <$> (fmap . fmap) toAsteroid ls
      (numAsteroids, baseLoc) =
        maximumBy (comparing fst) $
          (\loc -> (numIntersections grid loc, loc))
            <$> asteroidLocations grid
      orderedExplosions = explodeUntil baseLoc 200 grid
      (finalX, finalY) = last orderedExplosions
  return $ (numAsteroids, (100 * finalX) + finalY)
