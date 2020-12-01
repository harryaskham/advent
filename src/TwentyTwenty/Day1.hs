module TwentyTwenty.Day1 where

import Data.List (nub)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)

inputPath :: String
inputPath = "input/2020/1.txt"

part1 :: IO ()
part1 = do
  ls <- lines <$> readFile inputPath
  let xs = read <$> ls
  print $ [a * b | a <- xs, b <- xs, a + b == 2020]

-- O(n^2) solution using precomputed all-pairs distance-to-2020
part2 = do
  ls <- lines <$> readFile inputPath
  let xs = read <$> ls
      differenceLookup = M.fromList $ (\a b -> (2020 - a - b, (a, b))) <$> xs <*> xs
      get a =
        case M.lookup a differenceLookup of
          Just (b, c) -> Just (a * b * c)
          Nothing -> Nothing
  print $ catMaybes $ get <$> xs
