module Day12 (part1, part2) where

import Data.Char (isLower, isUpper)
import Data.Map.Strict qualified as M
import Data.Sequence (Seq (Empty, (:<|)), singleton, (><))
import Data.Sequence qualified as SQ
import Data.Set qualified as S
import Helper.TH (input)
import Helper.Util (adjustWithDefault, count, parseLinesWith, toTuple2)
import Text.ParserCombinators.Parsec (GenParser, char, letter, many1, sepBy)

data Node = Start | End | Big String | Small String deriving (Eq, Ord, Show)

toNode :: String -> Maybe Node
toNode "start" = Just Start
toNode "end" = Just End
toNode n
  | all isUpper n = Just (Big n)
  | all isLower n = Just (Small n)
  | otherwise = Nothing

isSmall :: Node -> Bool
isSmall (Small _) = True
isSmall _ = False

graph :: Map Node [Node]
graph =
  $(input 12)
    & parseLinesWith (toTuple2 <$> (mapMaybe toNode <$> many1 letter `sepBy` char '-'))
    & concatMap (\(a, b) -> [(a, [b]), (b, [a])])
    & M.fromListWith (++)

allPaths :: (Map Node Int -> Bool) -> Map Node [Node] -> [[Node]]
allPaths validCounts g = go (singleton (Start, [], M.empty)) S.empty []
  where
    go Empty _ paths = paths
    go ((current, path, counts) :<| queue) seen paths
      | current == End = go queue seen' (path' : paths)
      | (current, path) `S.member` seen = go queue seen paths
      | current == Start && not (null path) = go queue seen paths
      | validCounts counts' = go queue' seen' paths
      | otherwise = go queue seen paths
      where
        path' = current : path
        seen' = S.insert (current, path) seen
        counts' = adjustWithDefault 0 (+ 1) current counts
        queue' = queue >< SQ.fromList ((,path',counts') <$> catMaybes (sequence (M.lookup current g)))

solve :: (Map Node Int -> Bool) -> Int
solve f = graph & allPaths f & length

part1 :: Int
part1 =
  solve
    ( M.filterWithKey (\k v -> isSmall k && v > 1)
        >>> M.size
        >>> (== 0)
    )

part2 :: Int
part2 =
  solve
    ( M.filterWithKey (\k _ -> isSmall k)
        >>> M.elems
        >>> (((<= 1) . count (== 2)) &&& (all (== 1) . filter (/= 2)))
        >>> (== (True, True))
    )
