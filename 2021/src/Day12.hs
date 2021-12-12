module Day12 (part1, part2) where

import Data.Char (isLower, isUpper)
import Data.Map.Strict qualified as M
import Data.Sequence qualified as SQ
import Data.Set qualified as S
import Helper.TH (input)
import Helper.Util (adjustWithDefault, count, parseLinesWith, toTuple2)
import Text.ParserCombinators.Parsec (GenParser, char, letter, many1, sepBy)

data Node
  = Start
  | End
  | Big String
  | Small String
  deriving (Eq, Ord, Show)

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

line :: GenParser Char () (Node, Node)
line = toTuple2 <$> (mapMaybe toNode <$> many1 letter `sepBy` char '-')

allPaths :: (Map Node Int -> Bool) -> Map Node [Node] -> [[Node]]
allPaths validVisitCounts g = go (SQ.singleton (Start, [], M.empty)) S.empty []
  where
    go SQ.Empty _ paths = reverse <$> paths
    go (st@(current, path, visitCounts) SQ.:<| queue) seen paths
      | current == End = go queue seen' (path' : paths)
      | st `S.member` seen = go queue seen paths
      | current == Start && not (null path) = go queue seen paths
      | validVisitCounts visitCounts' = go queue' seen' paths
      | otherwise = go queue seen paths
      where
        path' = current : path
        seen' = S.insert st seen
        visitCounts' = adjustWithDefault 0 (+ 1) current visitCounts
        next = case M.lookup current g of
          Nothing -> SQ.Empty
          Just ns -> SQ.fromList (ns <&> (,path',visitCounts'))
        queue' = queue SQ.>< next

graph :: Map Node [Node]
graph =
  $(input 12)
    & parseLinesWith line
    & concatMap (\(a, b) -> [(a, [b]), (b, [a])])
    & M.fromListWith (++)

part1 :: Int
part1 = graph & allPaths ((== 0) . M.size . (M.filterWithKey (\k v -> isSmall k && v > 1))) & length

part2 :: Int
part2 = graph & allPaths ((== (True, True)) . (((<= 1) . Helper.Util.count (== 2)) &&& (all (== 1) . filter (/= 2))) . M.elems . M.filterWithKey (\k v -> isSmall k)) & length
