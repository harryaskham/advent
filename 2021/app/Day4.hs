module Day4 where

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

part1 :: IO ()
part1 = do
  xs <- readInput (signed decimal) (input 4)
  ys <- parseInput parser (input 4)
  zs <- lines <$> readFileText (input 4)
  print (xs :: [Int])
  print (ys :: [Int])
  print zs
