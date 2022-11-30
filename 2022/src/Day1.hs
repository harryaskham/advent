module Day1 (part1, part2) where

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
import Text.ParserCombinators.Parsec

elf :: Parser [Integer]
elf = many1 (number <* eol) <* eol

elves :: Parser [[Integer]]
elves = many1 elf <* eol

part1 :: Text
part1 =
  $(input 1)
    & parseWith elves
    & show

part2 :: Text
part2 = "Part 2"
