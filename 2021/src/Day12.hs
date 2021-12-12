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
allPaths validCounts g = go (singleton (Start, [], initCounts)) []
  where
    initCounts = 0 <$ M.filterWithKey (\k _ -> isSmall k) g
    go Empty paths = paths
    go ((current, path, counts) :<| queue) paths
      | current == End = go queue (path' : paths)
      | current == Start && not (null path) = go queue paths
      | validCounts counts' = go queue' paths
      | otherwise = go queue paths
      where
        path' = current : path
        counts' = if isSmall current then M.adjust (+ 1) current counts else counts
        queue' = queue >< SQ.fromList ((,path',counts') <$> catMaybes (sequence (M.lookup current g)))

solve :: (Map Node Int -> Bool) -> Int
solve validCounts = graph & allPaths validCounts & length

part1 :: Int
part1 = solve (M.elems >>> all (<= 1))

part2 :: Int
part2 = solve (M.elems >>> ((&&) <$> ((<= 1) . count (== 2)) <*> all (<= 2)))
