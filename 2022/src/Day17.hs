module Day17 (part1, part2) where

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
import Helper.Util
import Relude.Unsafe qualified as U
import Text.ParserCombinators.Parsec

pieces :: [[Coord2]]
pieces =
  [ [(0, 0), (1, 0), (2, 0), (3, 0)],
    [(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)],
    [(2, 2), (2, 1), (0, 0), (1, 0), (2, 0)],
    [(0, 0), (0, 1), (0, 2), (0, 3)],
    [(0, 0), (0, 1), (1, 0), (1, 1)]
  ]

dropPieces :: Grid SimpleWall -> [[Coord2]] -> String -> Grid SimpleWall
dropPieces g [] _ = g
dropPieces g (p : ps) moves =
  let (g', moves') = drop p' moves in dropPieces g' ps moves'
  where
    h = if M.null g then 3 else snd (maxXY g) + 4
    p' = bimap (+ 2) (+ h) <$> p
    drop cs (m : ms)
      -- any (\(_, y) -> y == 0) cs''' && not (any (\(x, y) -> (x, y) `M.member` g) cs''') = (foldl' (\g c -> M.insert c Wall g) g cs''', ms)
      | any (\(x, y) -> y == -1 || (x, y) `M.member` g) cs''' =
        traceShow (cs, m) $
          (foldl' (\g c -> M.insert c Wall g) g cs'', ms)
      | otherwise =
        -- (if M.null g then id else traceTextLn (T.unlines . reverse . T.lines . pretty $ fillEmpty (foldl' (\g c -> M.insert c Wall g) g cs))) $
        -- traceShow (cs, m) $
        -- pauseId $
        drop cs''' ms
      where
        cs' = case m of
          '<' -> first (subtract 1) <$> cs
          '>' -> first (+ 1) <$> cs
        cs'' = if any (\(x, y) -> x < 0 || x >= 7 || (x, y) `M.member` g) cs' then cs else cs'
        cs''' = second (subtract 1) <$> cs''

part1 :: Int
part1 =
  $(input 17)
    & lines
    & U.head
    & T.unpack
    & cycle
    & dropPieces M.empty (take 2022 $ cycle pieces)
    & maxXY
    & snd
    & (+ 1)

part2 :: Text
part2 = "Part 2"
