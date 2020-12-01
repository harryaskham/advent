module TwentyTwenty.Day1 where

import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)

inputPath :: String
inputPath = "input/2020/1.txt"

part1 :: IO ()
part1 = do
  xs <- fmap read . lines <$> readFile inputPath
  print $ [a * b | a <- xs, b <- xs, a + b == 2020]

-- O(n^2) solution using precomputed all-pairs distance-to-2020
part2 :: IO ()
part2 = do
  xs <- fmap read . lines <$> readFile inputPath
  let lookup = M.fromList $ (\a b -> (2020 - a - b, (a, b))) <$> xs <*> xs
      get a = (a *) . (uncurry (*)) <$> M.lookup a lookup
  print $ catMaybes $ get <$> xs
