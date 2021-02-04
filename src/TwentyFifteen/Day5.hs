module TwentyFifteen.Day5 where

import Data.List (intersect)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Util (countMap, input)

chunk2 :: [a] -> [(a, a)]
chunk2 [a, b] = [(a, b)]
chunk2 (a : b : xs) = (a, b) : chunk2 (b : xs)

isNice :: String -> Bool
isNice s = numVowels >= 3 && hasSame && noBlocked
  where
    cm = countMap s
    numVowels = sum (catMaybes (M.lookup <$> "aeiou" <*> pure cm))
    chunks = chunk2 s
    hasSame = not . null $ filter ((==) <$> fst <*> snd) chunks
    blocklist = [('a', 'b'), ('c', 'd'), ('p', 'q'), ('x', 'y')]
    noBlocked = null $ chunks `intersect` blocklist

solve :: (String -> Bool) -> IO Int
solve f = length . filter f . lines <$> input 2015 5

part1 :: IO Int
part1 = solve isNice

hasABA :: String -> Bool
hasABA [_, _] = False
hasABA (a : b : c : xs)
  | a == c = True
  | otherwise = hasABA (b : c : xs)

hasTwoSeparatedChunks :: String -> Bool
hasTwoSeparatedChunks s =
  not $
    null
      [ c1
        | (i1, c1) <- chunks,
          (i2, c2) <- chunks,
          i2 > i1 + 1,
          c1 == c2
      ]
  where
    chunks = zip [0 ..] (chunk2 s)

isNice2 :: String -> Bool
isNice2 = (&&) <$> hasABA <*> hasTwoSeparatedChunks

part2 :: IO Int
part2 = solve isNice2
