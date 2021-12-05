module Day5 (part1, part2, points) where

import Data.Map.Strict qualified as M
import Data.Tuple.Strict
import Helper.Coord (Coord2)
import Helper.TH (input)
import Helper.Util (both, countMap, eol, number, parseWith, toTuple2)
import Text.ParserCombinators.Parsec (GenParser, char, eof, many1, string)

parser :: GenParser Char () [(Coord2, Coord2)]
parser = many1 (line <* eol) <* eof
  where
    line = do
      x1 <- number <* char ','
      y1 <- number <* string " -> "
      x2 <- number <* char ','
      y2 <- number
      return . toTuple2 . sort $ [(x1, y1), (x2, y2)]

points :: (Coord2, Coord2) -> [Coord2]
points ((x1, y1), (x2, y2))
  | x1 == x2 || y1 == y2 = [(x, y) | x <- [x1 .. x2], y <- [y1 .. y2]]
  | otherwise = [(x, y) | x <- [x1 .. x2], let y = y1 + (x - x1) * signum (y2 - y1)]

solve :: ((Coord2, Coord2) -> Bool) -> Int
solve f =
  $(input 5)
    & parseWith parser
    & filter f
    & concatMap points
    & countMap
    & M.filter (> 1)
    & M.size

part1 :: Int
part1 = solve $ uncurry (||) . both (uncurry (==)) . uncurry zipPair

part2 :: Int
part2 = solve (const True)
