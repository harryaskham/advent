module TwentyTwenty.Day9 where

import Data.Maybe (catMaybes)
import qualified Data.Set as S
import qualified Data.Vector as V

inputPath :: String
inputPath = "input/2020/9.txt"

readInput :: IO [Int]
readInput = fmap read . lines <$> readFile inputPath

canSum :: [Int] -> Int -> Bool
canSum preamble x =
  or
    [ (x - p) `S.member` S.fromList preamble
      | p <- preamble,
        p /= x - p
    ]

findFirstInvalid :: [Int] -> Int
findFirstInvalid all@(_ : xs) =
  if not $ canSum preamble next
    then next
    else findFirstInvalid xs
  where
    preamble = take 25 all
    (next : _) = drop 25 all

part1 :: IO Int
part1 = findFirstInvalid <$> readInput

findSubsetFrom :: Int -> V.Vector Int -> Int -> Int -> Int -> Maybe [Int]
findSubsetFrom target xs i j s
  | s == target = Just $ V.toList $ V.slice i (j - i + 1) xs
  | s > target = Nothing
  | s < target = findSubsetFrom target xs i (j + 1) (s + (xs V.! (j + 1)))

findRange :: Int -> V.Vector Int -> [Int]
findRange target xs =
  head . catMaybes $
    [ findSubsetFrom target xs i (i + 1) (xs V.! i + xs V.! (i + 1))
      | i <- [0 .. V.length xs - 2]
    ]

part2 :: IO Int
part2 = do
  range <- findRange <$> part1 <*> (V.fromList <$> readInput)
  return $ maximum range + minimum range
