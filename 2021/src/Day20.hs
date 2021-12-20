module Day20 (part1, part2) where

import Data.Array qualified as A
import Data.Bimap (Bimap)
import Data.Bimap qualified as BM
import Data.List ((!!))
import Data.Map.Strict qualified as M
import Data.Mod
import Data.PQueue.Prio.Min qualified as PQ
import Data.Sequence qualified as SQ
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Read
import Data.Vector (Vector)
import Data.Vector qualified as V
import Helper.Bits (bitsToInt)
import Helper.Coord
import Helper.Grid
import Helper.TH
import Helper.Tracers
import Helper.Util
import Text.ParserCombinators.Parsec

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
  traceTextF pretty $
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

part1 :: Int
part1 =
  let (alg, grid) = parseWith parser $(input 20)
      enhances = [enhance def alg | def <- cycle [False, True]]
   in traceTextLn (pretty grid) $ M.size . M.filter (== True) . traceTextF pretty $ foldl' (&) grid (take 50 enhances)

part2 :: Text
part2 = "Part 2"
