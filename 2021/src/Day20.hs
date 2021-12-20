module Day20 (part1, part2) where

import Data.Bimap qualified as BM
import Data.List ((!!))
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Helper.Bits (bitsToInt)
import Helper.Grid (Grid, GridCell (charMap, fromChar), maxXY, minXY, pretty, readGrid)
import Helper.TH (input)
import Helper.Util (eol, parseWith)
import Text.ParserCombinators.Parsec (GenParser, eof, many1, oneOf)

type Cell = Bool

instance GridCell Bool where
  charMap = BM.fromList [(False, '.'), (True, '#')]

parser :: GenParser Char () (Vector Cell, Grid Cell)
parser = do
  a <- V.fromList <$> many1 (fromChar <$> oneOf ".#") <* eol
  eol
  let line = T.pack <$> (many1 (oneOf ".#") <* eol)
  gls <- many1 line
  eof
  return (a, readGrid (unlines gls))

enhance :: Cell -> Vector Cell -> Grid Cell -> Grid Cell
enhance def alg grid =
  M.fromList
    [ ((x, y), algChar x y)
      | x <- [x0 - 2 .. x1 + 2],
        y <- [y0 - 2 .. y1 + 2]
    ]
  where
    (x0, y0) = minXY grid
    (x1, y1) = maxXY grid
    algChar x y =
      (alg V.!) . fromIntegral . bitsToInt . concat $
        [ [ M.findWithDefault def (x', y') grid
            | x' <- [x -1 .. x + 1]
          ]
          | y' <- [y -1 .. y + 1]
        ]

solve :: Int -> Int
solve n =
  let (alg, grid) = parseWith parser $(input 20)
      enhances = [enhance def alg | def <- cycle [False, True]]
   in M.size . M.filter (== True) $ foldl' (&) grid (take n enhances)

part1 :: Int
part1 = solve 2

part2 :: Int
part2 = solve 50
