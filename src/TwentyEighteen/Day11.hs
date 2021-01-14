module TwentyEighteen.Day11 where

import Control.Parallel.Strategies (parMap, rdeepseq)
import Coord (Coord2, neighbors)
import Data.Char (digitToInt)
import Data.List (nub)
import Data.List.Extra (maximumOn)
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Tuple.Extra (thd3)

serial :: Int
serial = 6392

hundredsDigit :: Int -> Int
hundredsDigit x = let sx = show x in digitToInt $ sx !! (length sx - 3)

powerLevel :: Coord2 -> Int
powerLevel (x, y) = hundredsDigit ((((x + 10) * y) + serial) * (x + 10)) - 5

part1 :: (Int, Int)
part1 =
  snd . maximum $
    [ (sum $ powerLevel <$> (x, y) : neighbors (x, y), (x - 1, y - 1))
      | x <- [2 .. 299],
        y <- [2 .. 299]
    ]

runCoord' :: Int -> Int -> Int -> Map Int Int -> Map Int Int
runCoord' x y n nToPower
  | x + n > 300 || y + n > 300 = nToPower
  | otherwise = runCoord' x y (n + 1) (M.insert n power nToPower)
  where
    nextCoords =
      nub $
        [(x + n - 1, y + yO) | yO <- [0 .. n - 1]]
          ++ [(x + xO, y + n - 1) | xO <- [0 .. n - 1]]
    power = (nToPower M.! (n - 1)) + sum (powerLevel <$> nextCoords)

runCoord :: Int -> Int -> Map Int Int
runCoord x y = runCoord' x y 2 (M.singleton 1 (powerLevel (x, y)))

part2 :: (Int, Int, Int)
part2 =
  let coords = [(x, y) | x <- [1 .. 300], y <- [1 .. 300]]
      f (x, y) = (x, y, maximumOn snd (M.toList (runCoord x y)))
      results = parMap rdeepseq f coords
      (x, y, (n, _)) = maximumOn (snd . thd3) results
   in (x, y, n)
