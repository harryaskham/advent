module Day25 where

import qualified Data.Map.Strict as M
import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.Sequence as SQ
import qualified Data.Set as S
import Data.Text.Read
import Helper.Coord
import Helper.Grid
import Helper.Tracers
import Helper.Util
import Text.ParserCombinators.Parsec

parser :: GenParser Char () [Int]
parser = many1 (number <* eol) <* eof

part1 :: IO ()
part1 = do
  xs <- readInput (signed decimal) (input 25)
  ys <- parseInput parser (input 25)
  zs <- lines <$> readFileText (input 25)
  print (xs :: [Int])
  print (ys :: [Int])
  print zs
