module TwentyFifteen.Day17 where

import Data.List (delete, sortOn)
import Data.List.Extra (groupOn)
import qualified Data.Sequence as SQ
import Data.Set (Set)
import qualified Data.Set as S
import Util (input)

pack :: Int -> [Int] -> Set (Set (Int, Int))
pack target xs = go (SQ.singleton ((zip [0 ..] xs), S.empty)) S.empty S.empty
  where
    go SQ.Empty _ packings = packings
    go ((remaining, packed) SQ.:<| queue) seen packings
      | packed `S.member` seen = go queue seen packings
      | currentSum == target = go queue (S.insert packed seen) (S.insert packed packings)
      | currentSum > target = go queue (S.insert packed seen) packings
      | otherwise = go (queue SQ.>< nextStates) (S.insert packed seen) packings
      where
        currentSum = sum (snd <$> S.toList packed)
        nextStates = SQ.fromList [(delete r remaining, S.insert r packed) | r <- remaining]

part1 :: IO Int
part1 = do
  xs <- fmap read . lines <$> input 2015 17
  return $ S.size (pack 150 xs)

part2 :: IO Int
part2 = do
  xs <- fmap read . lines <$> input 2015 17
  let packings = S.toList $ pack 150 xs
  return . length . head . groupOn S.size . sortOn S.size $ packings
