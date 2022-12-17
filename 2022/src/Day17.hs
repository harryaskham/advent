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
      | any (\(x, y) -> y == -1 || (x, y) `M.member` g) cs''' =
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

oneLoop :: Int -> Grid SimpleWall -> [[Coord2]] -> Int -> String -> (Int, Int)
oneLoop cycLen g (p : ps) n moves
  | all (`M.member` g) ([(x, gh) | x <- [0 .. 6]]) && n `mod` cycLen == 0 =
    traceTextLn (T.unlines . reverse . T.lines . pretty $ fillEmpty g) $
      (n, gh)
  | otherwise =
    traceShow (cycLen, n) $
      let (g', moves') = drop p' moves in oneLoop cycLen g' ps (n + 1) moves'
  where
    gh = if M.null g then 0 else snd (maxXY g)
    h = if M.null g then 3 else snd (maxXY g) + 4
    p' = bimap (+ 2) (+ h) <$> p
    drop cs (m : ms)
      | any (\(x, y) -> y == -1 || (x, y) `M.member` g) cs''' =
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

-- Need to find a floor but also take into account where we're at in the piece rotation
-- and the move rotation! so a floor that is also multiple of 5 and number of moves?

part2 :: Int
part2 =
  let moves = $(input 17) & lines & U.head & T.unpack
      cycLen = lcm (length moves) (length pieces)
      (n, gh) = oneLoop cycLen M.empty (cycle pieces) 0 (cycle moves)
      loops = 1000000000000 `div` n
      toRun = 1000000000000 `mod` n
   in (dropPieces M.empty (take toRun $ cycle pieces) moves & maxXY & snd & (+ 1)) + (loops * (gh + 1))

-- 1571593533485  too high
