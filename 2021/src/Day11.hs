module Day11 where

import Data.Array qualified as A
import Data.Map.Strict qualified as M
import Data.PQueue.Prio.Min qualified as PQ
import Data.Sequence qualified as SQ
import Data.Set qualified as S
import Data.Text.Read
import Data.Vector qualified as V
import Helper.Coord
import Helper.Grid
import Helper.Tracers
import Helper.Util
import Text.ParserCombinators.Parsec

parser :: GenParser Char () [Int]
parser = many1 (number <* eol) <* eof

part1 :: IO Text
part1 = do
  xs <- readInput (signed decimal) (input 11)
  ys <- parseInput parser (input 11)
  zs <- lines <$> readFileText (input 11)
  print (xs :: [Int])
  print (ys :: [Int])
  print zs
  return "Part 1"

part2 :: IO Text
part2 = return "Part 2"
