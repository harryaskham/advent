module Day20 (part1, part2) where

import Data.Bimap qualified as BM
import Data.List ((!!))
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Helper.Bits (bitsToInt)
import Helper.Grid (Grid, GridCell (charMap, fromChar), maxXY, minXY, readGrid)
import Helper.TH (input)
import Helper.Util (eol, parseWith)
import Text.ParserCombinators.Parsec (GenParser, eof, many1, oneOf)

newtype Cell = Cell {uncell :: Bool} deriving (Eq, Ord)

instance GridCell Cell where
  charMap = BM.fromList [(Cell False, '.'), (Cell True, '#')]

parser :: GenParser Char () (Vector Cell, Grid Cell)
parser =
  (,)
    <$> (V.fromList <$> many1 (fromChar <$> oneOf ".#") <* (eol >> eol))
    <*> (readGrid . unlines <$> many1 (T.pack <$> (many1 (oneOf ".#") <* eol)) <* eof)

enhance :: Cell -> Vector Cell -> Grid Cell -> Grid Cell
enhance def alg grid =
  M.fromList [((x, y), algChar x y) | x <- [x0 - 2 .. x1 + 2], y <- [y0 - 2 .. y1 + 2]]
  where
    (x0, y0) = minXY grid
    (x1, y1) = maxXY grid
    algChar x y =
      (alg V.!) . fromIntegral . bitsToInt . fmap uncell . concat $
        [[M.findWithDefault def (x', y') grid | x' <- [x -1 .. x + 1]] | y' <- [y -1 .. y + 1]]

solve :: Int -> Int
solve n =
  let (alg, grid) = parseWith parser $(input 20)
      enhances = [enhance def alg | def <- cycle [Cell False, Cell True]]
   in M.size . M.filter (== Cell True) $ foldl' (&) grid (take n enhances)

part1 :: Int
part1 = solve 2

part2 :: Int
part2 = solve 50
