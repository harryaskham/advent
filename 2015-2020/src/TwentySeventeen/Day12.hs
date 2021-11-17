module TwentySeventeen.Day12 where

import Data.List (nub)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import qualified Data.Sequence as SQ
import qualified Data.Set as S

inputPath :: String
inputPath = "input/2017/12.txt"

type Graph = M.Map Int [Int]

edges :: String -> [(Int, [Int])]
edges l = (read x, read <$> ys) : [(read y, [read x]) | y <- ys]
  where
    [x, ys'] = splitOn " <-> " l
    ys = splitOn ", " ys'

visible :: Graph -> S.Set Int -> SQ.Seq Int -> S.Set Int
visible graph seen queue =
  case SQ.viewl queue of
    SQ.EmptyL -> seen
    current SQ.:< rest ->
      let nextSeen = S.insert current seen
          neighbours = filter (not . (`S.member` seen)) (graph M.! current)
       in visible graph nextSeen (rest SQ.>< SQ.fromList neighbours)

mkGraph :: IO Graph
mkGraph = do
  ls <- lines <$> readFile inputPath
  return $ nub <$> M.fromListWith (++) (edges =<< ls)

part1 :: IO Int
part1 = do
  graph <- mkGraph
  return $ S.size $ visible graph S.empty (SQ.singleton 0)

countGroups :: Graph -> S.Set Int -> Int -> Int
countGroups graph nodes n
  | S.null nodes = n
  | otherwise = countGroups graph (nodes S.\\ group) (n + 1)
  where
    start = head $ S.toList nodes
    group = visible graph S.empty (SQ.singleton start)

part2 :: IO Int
part2 = do
  graph <- mkGraph
  return $ countGroups graph (S.fromList $ M.keys graph) 0
