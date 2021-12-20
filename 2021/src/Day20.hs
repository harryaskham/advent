module Day20 (part1, part2) where

import Data.Bimap qualified as BM
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

nextCell :: Vector Cell -> Cell -> Grid Cell -> Int -> Int -> Cell
nextCell alg def grid x y =
  (alg V.!) . fromIntegral . bitsToInt . concat $
    [[uncell $ M.findWithDefault def (x', y') grid | x' <- [x - 1 .. x + 1]] | y' <- [y - 1 .. y + 1]]

enhance :: Vector Cell -> Cell -> Grid Cell -> Grid Cell
enhance alg def grid =
  let ((x0, y0), (x1, y1)) = (minXY &&& maxXY) grid
   in M.fromList [((x, y), nextCell alg def grid x y) | x <- [x0 - 1 .. x1 + 1], y <- [y0 - 1 .. y1 + 1]]

solve :: Int -> Int
solve n =
  let (alg, grid) = parseWith parser $(input 20)
      defs = case (V.head alg, V.last alg) of
        (Cell False, _) -> repeat (Cell False)
        (Cell True, Cell True) -> repeat (Cell True)
        (Cell True, Cell False) -> cycle [Cell False, Cell True]
   in M.size . M.filter (== Cell True) $ foldl' (&) grid (take n (enhance alg <$> defs))

part1 :: Int
part1 = solve 2

part2 :: Int
part2 = solve 50
