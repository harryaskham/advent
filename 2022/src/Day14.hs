module Day14 (part1, part2) where

import Data.List (maximum)
import Data.Map.Strict qualified as M
import Helper.Coord (Coord2)
import Helper.Grid (Grid, fillDef, fromCoords, maxXY)
import Helper.TH (input)
import Helper.Util (eol, number, parseWith, range, toTuple2)
import Text.ParserCombinators.Parsec (Parser, char, eof, many1, sepBy, string)

data Cell = Empty | Wall | Sand deriving (Eq, Ord, Bounded)

parser :: Parser [Coord2]
parser = concat <$> many1 (segment <* eol) <* eof
  where
    segment = toCoords <$> (toTuple2 <$> number `sepBy` char ',') `sepBy` string " -> "
    toCoords ss = [(x, y) | ((x1, y1), (x2, y2)) <- zip ss (drop 1 ss), x <- range x1 x2, y <- range y1 y2]

dropGrain :: Int -> Grid Cell -> Maybe (Grid Cell)
dropGrain h g = go (500, 0)
  where
    go (x, y)
      | y >= h = Nothing
      | M.lookup (500, 0) g == Just Sand = Nothing
      | M.lookup (x, y + 1) g == Just Empty = go (x, y + 1)
      | M.lookup (x - 1, y + 1) g == Just Empty = go (x - 1, y + 1)
      | M.lookup (x + 1, y + 1) g == Just Empty = go (x + 1, y + 1)
      | otherwise = Just (M.insert (x, y) Sand g)

solve :: [Coord2] -> Int
solve coords =
  let g = fromCoords Wall coords
      (_, h) = maxXY g
   in Just g
        & iterate (>>= dropGrain h)
        & takeWhile isJust
        & length
        & subtract 1

part1 :: Int
part1 = $(input 14) & parseWith parser & solve

part2 :: Int
part2 =
  let addFloor cs = cs ++ [(x, maximum (snd <$> cs) + 2) | x <- [0 .. 1000]]
   in $(input 14) & parseWith parser & addFloor & solve
