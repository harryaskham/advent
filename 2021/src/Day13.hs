module Day13 (part1, part2) where

import Control.Arrow (app)
import Data.List ((!!))
import Data.Set qualified as S
import Helper.Grid (SimpleWall (Wall), fromCoords, pretty)
import Helper.TH (input)
import Helper.Util (eol, number, parseWith, toTuple2)
import Text.ParserCombinators.Parsec (GenParser, char, eof, many1, sepBy1, string)

parser :: GenParser Char () ([Set (Int, Int) -> Set (Int, Int)], Set (Int, Int))
parser = do
  let fX = string "x=" *> ((\c -> S.map (\(x, y) -> if x > c then (c * 2 - x, y) else (x, y))) <$> number)
      fY = string "y=" *> ((\c -> S.map (\(x, y) -> if y > c then (x, c * 2 - y) else (x, y))) <$> number)
  dots <- many1 (toTuple2 <$> number `sepBy1` char ',' <* eol) <* eol
  folds <- many1 ((string "fold along " >> (fX <|> fY)) <* eol) <* eof
  return (folds, S.fromList dots)

part1 :: Int
part1 = $(input 13) & parseWith parser & first (!! 0) & app & S.size

part2 :: Text
part2 = $(input 13) & parseWith parser & uncurry (flip (foldl' (&))) & fromCoords Wall & pretty
