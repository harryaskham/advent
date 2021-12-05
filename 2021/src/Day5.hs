module Day5 (part1, part2) where

import Data.Map.Strict qualified as M
import Data.Tuple.Strict (zipPair)
import Helper.Coord (Coord2, linePoints)
import Helper.TH (input)
import Helper.Util (both, countMap, eol, number, pair, parseWith, toTuple2)
import Text.ParserCombinators.Parsec (GenParser, char, eof, many1, string)

parser :: GenParser Char () [(Coord2, Coord2)]
parser = many1 (line <* eol) <* eof
  where
    line =
      fmap (toTuple2 . sort) . pair
        <$> ((,) <$> (number <* char ',') <*> (number <* string " -> "))
        <*> ((,) <$> (number <* char ',') <*> number)

solve :: ((Coord2, Coord2) -> Bool) -> Int
solve f =
  $(input 5)
    & parseWith parser
    & filter f
    & concatMap linePoints
    & countMap
    & M.filter (> 1)
    & M.size

part1 :: Int
part1 = solve $ uncurry (||) . both (uncurry (==)) . uncurry zipPair

part2 :: Int
part2 = solve (const True)
