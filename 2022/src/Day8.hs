module Day8 (part1, part2) where

import Data.Array qualified as A
import Data.Bimap (Bimap)
import Data.Bimap qualified as BM
import Data.Char (intToDigit)
import Data.List (maximum)
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
import Text.ParserCombinators.Parsec

newtype Cell = Cell Int deriving (Eq, Ord)

instance GridCell Cell where
  charMap = BM.fromList [(Cell x, intToDigit x) | x <- [0 .. 9]]

visible :: Grid Cell -> Coord2 -> Bool
visible g (x, y) = visibleFrom `any` [above, below, left, right]
  where
    (w, h) = maxXY g
    above = [(x, y') | y' <- [0 .. y - 1]]
    below = [(x, y') | y' <- [y + 1 .. h]]
    left = [(x', y) | x' <- [0 .. x - 1]]
    right = [(x', y) | x' <- [x + 1 .. w]]
    visibleFrom = all ((< g M.! (x, y)) . (g M.!))

allVisible :: Grid Cell -> [Coord2]
allVisible g = [c | c <- M.keys g, visible g c]

scenicScore :: Grid Cell -> Coord2 -> Int
scenicScore g (x, y) = product [above, below, left, right]
  where
    (w, h) = maxXY g
    distance =
      fst
        . foldl'
          ( \(n, stop) c ->
              if stop
                then (n, stop)
                else
                  if g M.! c < g M.! (x, y)
                    then (n + 1, False)
                    else (n, True)
          )
          (1, False)
    above = distance [(x, y') | y' <- [y - 1, y - 2 .. 1]]
    below = distance [(x, y') | y' <- [y + 1 .. h - 1]]
    left = distance [(x', y) | x' <- [x - 1, x -2 .. 1]]
    right = distance [(x', y) | x' <- [x + 1 .. w - 1]]

maxScenicScore :: Grid Cell -> Int
maxScenicScore g =
  maximum
    [ scenicScore g c
      | let (w, h) = maxXY g,
        c@(x, y) <- M.keys g,
        x > 0,
        y > 0,
        x < w,
        y < h
    ]

part1 :: Int
part1 =
  $(input 8)
    & readGrid
    & allVisible
    & length

part2 :: Int
part2 =
  $(input 8)
    & readGrid
    & maxScenicScore
