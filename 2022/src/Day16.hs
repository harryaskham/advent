module Day16 (part1, part2) where

import Data.Array qualified as A
import Data.Bimap (Bimap)
import Data.Bimap qualified as BM
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.List (foldl1, maximum)
import Data.Map.Strict qualified as M
import Data.Mod
import Data.PQueue.Prio.Min qualified as PQ
import Data.Sequence qualified as SQ
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Read
import Data.Vector qualified as V
import Helper.Coord
import Helper.Grid
import Helper.TH
import Helper.Tracers
import Helper.Util hiding (count)
import Relude.Unsafe qualified as U
import Text.ParserCombinators.Parsec hiding (State)
import Prelude hiding ((<|>))

parser :: Parser (Map String (Int, [String]))
parser = M.fromList <$> many1 (line <* eol) <* eof
  where
    line = do
      vid <- string "Valve " *> count 2 anyChar
      rate <- string " has flow rate=" *> number
      ns <- (try (string "; tunnels lead to valves ") <|> try (string "; tunnel leads to valve ")) *> count 2 anyChar `sepBy` string ", "
      return (vid, (rate, ns))

mostPressure :: Map String (Int, [String]) -> Int
mostPressure g = go (SQ.singleton ("AA", S.empty, 0, 0, 0)) M.empty 0
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

mostPressure2 :: Map String (Int, [String]) -> Int
mostPressure2 g = go (SQ.singleton (("AA", "AA"), HS.empty, 0, 0, 0)) M.empty 0
  where
    go SQ.Empty _ best = best
    go ((_, _, _, total, 26) SQ.:<| rest) cache best = go rest cache (max total best)
    go (((currentA, currentB), open, flow, total, t) SQ.:<| queue) cache best
      | cacheKey `M.member` cache && cache M.! cacheKey >= cacheValue = go queue cache best
      | cacheKey2 `M.member` cache && cache M.! cacheKey2 >= cacheValue = go queue cache best
      | otherwise = traceShow (M.size cache, currentA, currentB, HS.size open, flow, total, t) $ go queue' (M.insert cacheKey cacheValue cache) best
      where
        cacheKey = (currentA, currentB, open)
        cacheKey2 = (currentB, currentA, open)
        cacheValue = total
        (currentFlowA, nsA) = g M.! currentA
        (currentFlowB, nsB) = g M.! currentB
        bothMove = [((nA, nB), open, flow, total + flow, t + 1) | nA <- nsA, nB <- nsB]
        aOpens = [((currentA, nB), HS.insert currentA open, flow + currentFlowA, total + flow, t + 1) | nB <- nsB]
        bOpens = [((nA, currentB), HS.insert currentB open, flow + currentFlowB, total + flow, t + 1) | nA <- nsA]
        bothOpen = ((currentA, currentB), HS.insert currentA . HS.insert currentB $ open, flow + currentFlowA + currentFlowB, total + flow, t + 1)
        next =
          concat
            [ if currentFlowA > 0 && not (currentA `HS.member` open) then aOpens else [],
              if currentFlowB > 0 && not (currentB `HS.member` open) then bOpens else [],
              if ( currentFlowA /= currentFlowB
                     && currentFlowA > 0
                     && currentFlowB > 0
                     && not (currentA `HS.member` open)
                     && not (currentB `HS.member` open)
                 )
                then [bothOpen]
                else [],
              bothMove
            ]
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

-- TODO: don't visit nodes where we would not be the first to open
-- TODO: some caching
-- TODO: example input value is wrong, we are missing it
-- TODO: THE SHORTEST PATHS ARE BROKE
mostPressure3 :: Map String (Int, [String]) -> Int
mostPressure3 g = go (SQ.singleton ("AA", "AA", 0, 0, M.empty, S.empty)) S.empty 0
  where
    paths = shortestPaths g
    go SQ.Empty _ best = best
    go ((currentA, currentB, tA, tB, open, seen) SQ.:<| queue) cache best
      | M.size open == M.size paths - 1 = go queue cache (max best (score g open))
      | tA >= 26 && tB >= 26 = go queue cache (max best (score g open))
      | cacheKey `S.member` cache = go queue cache best
      | (currentA, currentB) `S.member` seen = go queue cache best
      | otherwise =
        traceShow (currentA, currentB, tA, tB, M.size open, best) $ go queue' cache' best
      where
        cacheKey = (currentA, currentB, open)
        cacheKey2 = (currentB, currentA, open)
        cache' = S.insert cacheKey . S.insert cacheKey2 $ cache
        nsA = M.toList (paths M.! currentA)
        nsB = M.toList (paths M.! currentB)
        ins = M.insertWith min
        seen' = S.insert (currentA, currentB) seen
        bothMoveAndOpen =
          [ (nA, nB, tA + dA + 1, tB + dB + 1, ins nA (tA + dA + 1) . ins nB (tB + dB + 1) $ open, seen')
            | (nA, dA) <- nsA,
              (nB, dB) <- nsB,
              nA /= currentA,
              nB /= currentB,
              tA + dA + 1 < 26,
              tB + dB + 1 < 26
          ]
        aMoveAndOpen =
          [ (nA, currentB, tA + dA + 1, tB, ins nA (tA + dA + 1) open, seen')
            | (nA, dA) <- nsA,
              nA /= currentA,
              tA + dA + 1 < 26
          ]
        bMoveAndOpen =
          [ (currentA, nB, tA, tB + dB + 1, ins nB (tB + dB + 1) open, seen')
            | (nB, dB) <- nsB,
              nB /= currentB,
              tB + dB + 1 < 26
          ]
        aWaits = (currentA, currentB, tA + 1, tB, open, seen')
        bWaits = (currentA, currentB, tA, tB + 1, open, seen')
        next = aWaits : bWaits : (bothMoveAndOpen ++ aMoveAndOpen ++ bMoveAndOpen)
        queue' = queue

mostPressure4 :: Map String (Int, [String]) -> Int
mostPressure4 g = maximum [traceShowId $ score g $ M.unionWith min pA pB | pA <- thePaths, pB <- thePaths]
  where
    -- bestPath = U.last $ sortOn (score g) singlePaths
    singlePaths = S.toList $ go (SQ.singleton ("AA", 0, M.empty)) S.empty
    bestPaths = take 250 $ sortOn (Down . score g) singlePaths
    thePaths = bestPaths
    paths = shortestPaths g
    go SQ.Empty opens = opens
    go ((current, t, open) SQ.:<| queue) opens
      | M.size open == M.size paths - 1 = go queue (S.insert open opens)
      | t >= 26 = go queue (S.insert open opens)
      | otherwise =
        traceShow (current, t, M.size open) $ go queue' opens
      where
        ns = M.toList (paths M.! current)
        next = [(n, t + d + 1, M.insert n (t + d + 1) open) | (n, d) <- ns, not (n `M.member` open)]
        queue' = queue SQ.>< SQ.fromList next

-- DFS in state monad
-- can terminate once all valves are open

dfs :: Map String (Int, [String]) -> State (Map (String, String, HashSet String) Int) (Maybe Int)
dfs g = go "AA" "AA" HS.empty 0 0 0
  where
    go :: String -> String -> HashSet String -> Int -> Int -> Int -> State (Map (String, String, HashSet String) Int) (Maybe Int)
    go _ _ _ _ total 26 = return (Just total)
    go currentA currentB open flow total t = traceShow (currentA, currentB, HS.size open, flow, total, t) $ do
      let cacheKey = (currentA, currentB, open)
      cv <- gets (M.lookup cacheKey)
      let (currentFlowA, nsA) = g M.! currentA
      let (currentFlowB, nsB) = g M.! currentB
      let bothMove = [((nA, nB), open, flow, total + flow, t + 1) | nA <- nsA, nB <- nsB]
      let aOpens = [((currentA, nB), HS.insert currentA open, flow + currentFlowA, total + flow, t + 1) | nB <- nsB]
      let bOpens = [((nA, currentB), HS.insert currentB open, flow + currentFlowB, total + flow, t + 1) | nA <- nsA]
      let bothOpen = ((currentA, currentB), HS.insert currentA . HS.insert currentB $ open, flow + currentFlowA + currentFlowB, total + flow, t + 1)
      let next =
            concat
              [ if currentFlowA > 0 && not (currentA `HS.member` open) then aOpens else [],
                if currentFlowB > 0 && not (currentB `HS.member` open) then bOpens else [],
                if ( currentFlowA /= currentFlowB
                       && currentFlowA > 0
                       && currentFlowB > 0
                       && not (currentA `HS.member` open)
                       && not (currentB `HS.member` open)
                   )
                  then [bothOpen]
                  else [],
                bothMove
              ]
      let continue = do
            results <- traverse (\((a, b), s, f, to, ti) -> go a b s f to ti) next
            return (maximum results)
      case cv of
        Just oldTotal -> if oldTotal >= total then traceShow "cache hit" $ return Nothing else continue
        Nothing -> continue

part1 :: Int
part1 =
  $(input 16)
    & parseWith parser
    & mostPressure

part2 :: Int
part2 =
  $(input 16)
    & parseWith parser
    & mostPressure4

-- & mostPressure2

--part2 :: Maybe Int
--part2 =
--  $(exampleInput 16)
--    & parseWith parser
--    & dfs
--    & flip evalState M.empty
