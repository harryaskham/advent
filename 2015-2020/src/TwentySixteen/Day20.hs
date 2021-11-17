module TwentySixteen.Day20 where

import Data.List (sortOn)
import Data.List.Extra (splitOn)
import Util (input, toTuple2, (<$$>))

lowestUnblocked :: [(Int, Int)] -> Int
lowestUnblocked blacklist = go (sortOn fst blacklist)
  where
    go [(_, u)] = u + 1
    go ((l1, u1) : (l2, u2) : rest)
      | l2 > u1 + 1 = u1 + 1
      | u2 < u1 = go ((l1, u1) : rest)
      | otherwise = go ((l2, u2) : rest)

numAllowed :: [(Int, Int)] -> Int
numAllowed blacklist = go (sortOn fst blacklist)
  where
    go [(_, u)] = 2 ^ 32 - 1 - u
    go ((l1, u1) : (l2, u2) : rest)
      | l2 > u1 = l2 - u1 - 1 + go ((l2, u2) : rest)
      | u2 < u1 = go ((l1, u1) : rest)
      | otherwise = go ((l2, u2) : rest)

part12 :: IO (Int, Int)
part12 = do
  is <- (toTuple2 <$>) . (read <$$>) . (splitOn "-" <$>) . lines <$> input 2016 20
  return (lowestUnblocked is, numAllowed is)
