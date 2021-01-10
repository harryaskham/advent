module TwentyEighteen.Day3 where

import Data.Function ((&))
import Data.List (delete)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Util (countMap)

-- The details of a fabric rectangle
data FabRect = FabRect
  { _id :: String,
    _xC :: Int,
    _yC :: Int,
    _width :: Int,
    _height :: Int
  }
  deriving (Show)

-- Read a rectangle from #id @ x,y: wxh form.
parseFabricRect :: String -> FabRect
parseFabricRect s = FabRect idStr (read xStr) (read yStr) (read widthStr) (read heightStr)
  where
    [_ : idStr, _, coordStr, dimStr] = words s
    [xStr, yStr] = splitOn "," $ delete ':' coordStr
    [widthStr, heightStr] = splitOn "x" dimStr

-- Keep track of which coords have been covered how many times.
-- Pass in the current counts for each location and the current coord to update it with.
trackOverlap :: M.Map (Int, Int) Int -> (Int, Int) -> M.Map (Int, Int) Int
trackOverlap counts coord = M.insertWith (+) coord 1 counts

coordsForRect :: FabRect -> [(Int, Int)]
coordsForRect fr = [(x, y) | x <- [_xC fr .. _xC fr + _width fr - 1], y <- [_yC fr .. _yC fr + _height fr - 1]]

coordCounts :: [FabRect] -> M.Map (Int, Int) Int
coordCounts frs = countMap coords
  where
    coords = concat $ coordsForRect <$> frs

part1 :: IO Int
part1 = do
  frs <- fmap parseFabricRect . lines <$> readFile "input/2018/3.txt"
  return $ snd <$> M.toList (coordCounts frs) & filter (> 1) & length

-- Is the given rectangle non-overlapping?
isRectUnique :: M.Map (Int, Int) Int -> FabRect -> Bool
isRectUnique coordCounts rect = all (== 1) counts
  where
    coords = coordsForRect rect
    counts = catMaybes $ (`M.lookup` coordCounts) <$> coords

part2 :: IO String
part2 = do
  frs <- fmap parseFabricRect . lines <$> readFile "input/2018/3.txt"
  return $ _id . head $ filter (isRectUnique (coordCounts frs)) frs
