module Day16 (part1, part2) where

import Data.List (maximum)
import Data.Map.Strict qualified as M
import Data.Sequence qualified as SQ
import Data.Set qualified as S
import Helper.TH (input)
import Helper.Util (eol, number, parseWith)
import Text.ParserCombinators.Parsec (Parser, anyChar, count, eof, many1, sepBy, string, try, (<|>))
import Prelude hiding ((<|>))

parser :: Parser (Map String (Int, [String]))
parser = M.fromList <$> many1 (line <* eol) <* eof
  where
    line = do
      vid <- string "Valve " *> count 2 anyChar
      rate <- string " has flow rate=" *> number
      ns <- (try (string "; tunnels lead to valves ") <|> try (string "; tunnel leads to valve ")) *> count 2 anyChar `sepBy` string ", "
      return (vid, (rate, ns))

mostPressure' :: Map String (Int, [String]) -> Int
mostPressure' g = go (SQ.singleton ("AA", S.empty, 0, 0, 0)) M.empty 0
  where
    go SQ.Empty _ best = best
    go ((_, _, _, total, 30) SQ.:<| rest) cache best = go rest cache (max total best)
    go ((current, open, flow, total, t) SQ.:<| queue) cache best
      | cacheKey `M.member` cache && cache M.! cacheKey >= cacheValue = go queue cache best
      | otherwise = go queue' (M.insert cacheKey cacheValue cache) best
      where
        cacheKey = (current, open)
        cacheValue = total
        (currentFlow, ns) = g M.! current
        openCurrent = (current, S.insert current open, flow + currentFlow, total + flow, t + 1)
        moves = [(n, open, flow, total + flow, t + 1) | n <- ns]
        next = if currentFlow > 0 && current `S.notMember` open then openCurrent : moves else moves
        queue' = queue SQ.>< SQ.fromList next

shortestPaths :: Map String (Int, [String]) -> Map String (Map String Int)
shortestPaths g = M.fromList ([(n, startFrom n) | n <- "AA" : S.toList nodesWithFlow])
  where
    nodesWithFlow = S.fromList . M.keys $ M.filter (\(flow, _) -> flow > 0) g
    startFrom start = go (SQ.singleton (start, 0, S.empty)) M.empty
      where
        go SQ.Empty paths = paths
        go ((current, n, seen) SQ.:<| queue) paths
          | current `S.member` seen = go queue paths
          | otherwise = go queue' paths'
          where
            seen' = S.insert current seen
            paths' = if current `S.member` nodesWithFlow then M.insertWith min current n paths else paths
            (_, currentNs) = g M.! current
            next = [(node, n + 1, seen') | node <- currentNs]
            queue' = queue SQ.>< SQ.fromList next

score :: Map String (Int, [String]) -> Map String Int -> Int
score g open = sum [f * (26 - t) | (n, t) <- M.toList open, let (f, _) = g M.! n]

singlePaths :: Int -> Map String (Int, [String]) -> [Map String Int]
singlePaths limit g = S.toList $ go (SQ.singleton ("AA", 0, M.empty)) S.empty
  where
    paths = shortestPaths g
    go SQ.Empty opens = opens
    go ((current, t, open) SQ.:<| queue) opens
      | M.size open == M.size paths - 1 = go queue (S.insert open opens)
      | t >= limit = go queue (S.insert open opens)
      | otherwise = go queue' opens
      where
        ns = M.toList (paths M.! current)
        next = [(n, t + d + 1, M.insert n (t + d + 1) open) | (n, d) <- ns, not (n `M.member` open)]
        queue' = queue SQ.>< SQ.fromList next

-- TODO: Not working?
mostPressure :: Map String (Int, [String]) -> Int
mostPressure g = maximum [score g p | p <- singlePaths 30 g]

mostPressurePaired :: Map String (Int, [String]) -> Int
mostPressurePaired g =
  let bestPaths = take 250 $ sortOn (Down . score g) (singlePaths 26 g)
   in maximum [score g $ M.unionWith min pA pB | pA <- bestPaths, pB <- bestPaths]

part1 :: Int
part1 = $(input 16) & parseWith parser & mostPressure'

part2 :: Int
part2 = $(input 16) & parseWith parser & mostPressurePaired
