module TwentySeventeen.Day6 where

import Data.List (maximumBy)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Monoid (Sum (Sum, getSum))
import Data.Ord (comparing)

input :: [Int]
input = read <$> splitOn " " "10 3 15 10 5 15 5 15 9 2 5 8 5 2 3 6"

redistribute :: M.Map Int Int -> M.Map Int Int
redistribute banks = getSum <$> M.unionWith (<>) new old
  where
    getMaxBank (a, b) = (b, negate a)
    (maxBank, maxVal) = maximumBy (comparing getMaxBank) $ M.toList banks
    old = Sum <$> M.insert maxBank 0 banks
    new =
      M.fromListWith
        (<>)
        [ ((maxBank + offset) `mod` length banks, Sum 1)
          | offset <- [1 .. maxVal]
        ]

run :: M.Map Int Int -> M.Map (M.Map Int Int) Int -> Int -> (Int, Int)
run banks lastSeen steps
  | banks `M.member` lastSeen = (steps, steps - lastSeen M.! banks)
  | otherwise =
    run
      (redistribute banks)
      (M.insert banks steps lastSeen)
      (steps + 1)

part12 :: (Int, Int)
part12 = run (M.fromList (zip [0 ..] input)) M.empty 0
