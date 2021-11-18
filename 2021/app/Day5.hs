module Day5 where

import qualified Data.Array as A
import qualified Data.Map.Strict as M
import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.Sequence as SQ
import qualified Data.Set as S
import Data.Text.Read
import qualified Data.Vector as V
import Helper.Coord
import Helper.Grid
import Helper.Tracers
import Helper.Util
import Text.ParserCombinators.Parsec

parser :: GenParser Char () [Int]
parser = many1 (number <* eol) <* eof

part1 :: IO ()
part1 = do
  xs <- readInput (signed decimal) (input 5)
  ys <- parseInput parser (input 5)
  zs <- lines <$> readFileText (input 5)
  print (xs :: [Int])
  print (ys :: [Int])
  print zs
