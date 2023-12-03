module Day3 (part1, part2) where

import Data.Array qualified as A
import Data.Bimap (Bimap)
import Data.Bimap qualified as BM
import Data.Char (digitToInt, intToDigit)
import Data.List (foldl1', nub)
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

-- parser :: Parser [Int]
-- parser = many1 (number <* eol) <* eof

-- line :: Parser Int
-- line = number

data Cell
  = None
  | Digit Char
  | Mark Char
  deriving (Eq, Ord, Show)

instance GridCell Cell where
  charMap =
    BM.fromList $
      [(None, '.')]
        <> [(Digit i, i) | i <- ['0' .. '9']]
        <> [(Mark c, c) | c <- "!@#$%^&*()-=/+"]

getNumbers :: Grid Cell -> [(Int, [Coord2])]
getNumbers g = go 0 0 "" []
  where
    (maxX, maxY) = maxXY g
    go x y currentNum currentCoords
      | y > maxY = []
      | x > maxX =
          case (currentNum, currentCoords) of
            (s@(_ : _), cs@(_ : _)) -> (U.read s, nub cs) : go 0 (y + 1) "" []
            _ -> go 0 (y + 1) "" []
      | otherwise =
          case g M.! (x, y) of
            Digit c -> go (x + 1) y (currentNum <> [c]) (currentCoords <> neighbors (x, y))
            _ ->
              case (currentNum, currentCoords) of
                (s@(_ : _), cs@(_ : _)) -> (U.read s, nub cs) : go (x + 1) y "" []
                _ -> go (x + 1) y "" []

isMark :: Cell -> Bool
isMark (Mark _) = True
isMark _ = False

part1 :: Int
part1 =
  $(input 3)
    -- \$(exampleInput 3)
    & readGrid
    & ( \g ->
          getNumbers g
            & filter (\(_, cs) -> any isMark $ catMaybes [M.lookup c g | c <- cs])
      )
    & traceShowId
    & fmap fst
    & sum

part2 :: Int
part2 =
  $(input 3)
    -- \$(exampleInput 3)
    & readGrid
    & ( \g ->
          getNumbers g
            & fmap (\(n, cs) -> M.fromList [(c, [n]) | c <- cs, let a = M.lookup c g, a == Just (Mark '*')])
      )
    & foldl1' (M.unionWith (<>))
    & M.filter ((== 2) . length)
    & fmap (\[a, b] -> a * b)
    & traceShowId
    & M.toList
    & fmap snd
    & sum