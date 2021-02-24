module TwentyFifteen.Day24 where

import Combinatorics (tuples)
import Data.List (delete, foldl', permutations, sortOn, (\\))
import Data.Maybe (isJust)
import Data.Sequence (Seq)
import qualified Data.Sequence as SQ
import Data.Set (Set)
import qualified Data.Set as S
import Util (input)

partitionWithSum :: Int -> Int -> [Int] -> Maybe ([[Int]])
partitionWithSum n target xs
  | sum xs == n * target = go (SQ.singleton (xs, replicate n S.empty)) S.empty
  | otherwise = Nothing
  where
    go :: (Seq ([Int], [Set Int])) -> Set ([Set Int]) -> Maybe ([[Int]])
    go SQ.Empty _ = Nothing
    go ((remaining, partitions) SQ.:<| queue) seen
      | null remaining && all (== target) sps = Just (S.toList <$> partitions)
      | partitions `S.member` seen = go queue seen
      | null remaining = go queue nextSeen
      | any (> target) sps = go queue nextSeen
      | otherwise = go nextQueue nextSeen
      where
        sps = sum <$> partitions
        nextSeen = foldl' (flip S.insert) seen (permutations partitions)
        nextStates =
          SQ.fromList . concat $
            [ [ (rs, [if i == i' then S.insert r p else p | (i', p) <- zip [0 ..] partitions])
                | i <- [0 .. n -1]
              ]
              | r <- remaining,
                let rs = delete r remaining
            ]
        nextQueue = nextStates SQ.>< queue

solve :: Int -> IO Int
solve n = do
  xs <- fmap read . lines <$> input 2015 24
  return $
    head
      [ product g
        | g <- concat [sortOn product (tuples n xs) | n <- [1 ..]],
          canAllocate g (xs \\ g)
      ]
  where
    canAllocate g1 xs = isJust $ partitionWithSum n (sum g1) xs

part1 :: IO Int
part1 = solve 2

part2 :: IO Int
part2 = solve 3
