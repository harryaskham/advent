module Day16 (part1, part2) where

import Data.Array qualified as A
import Data.Bimap (Bimap)
import Data.Bimap qualified as BM
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
      ns <- (try (string "; tunnels lead to valves ") <|> (try (string "; tunnel leads to valve "))) *> count 2 anyChar `sepBy` string ", "
      return (vid, (rate, ns))

mostPressure :: Map String (Int, [String]) -> Int
mostPressure g = go (SQ.singleton ("AA", S.empty, 0, 0, 0)) S.empty 0
  where
    go SQ.Empty _ best = best
    go ((_, _, _, total, 30) SQ.:<| rest) seen best = go rest seen (max total best)
    go (st@(current, open, flow, total, t) SQ.:<| queue) seen best
      | st `S.member` seen = go queue seen best
      | otherwise = traceShow (current, S.size open, flow, total, t) $ go queue' (S.insert st seen) best
      where
        (currentFlow, ns) = g M.! current
        openCurrent = (current, S.insert current open, flow + currentFlow, total + flow, t + 1)
        moves = [(n, open, flow, total + flow, t + 1) | n <- ns]
        next = if current `S.member` open then moves else openCurrent : moves
        queue' = queue SQ.>< SQ.fromList next

part1 :: Int
part1 =
  $(input 16)
    & parseWith parser
    & mostPressure

part2 :: Text
part2 = "Part 2"
