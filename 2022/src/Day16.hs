module Day16 (part1, part2) where

import Data.Array qualified as A
import Data.Bimap (Bimap)
import Data.Bimap qualified as BM
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
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
import Text.ParserCombinators.Parsec
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

part1 :: Int
part1 =
  $(input 16)
    & parseWith parser
    & mostPressure

part2 :: Int
part2 =
  $(input 16)
    & parseWith parser
    & mostPressure2
