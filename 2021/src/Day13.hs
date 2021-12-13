module Day13 (part1, part2) where

import Data.List ((!!))
import Data.Set qualified as S
import Helper.Grid (SimpleWall (Wall), fromCoords, pretty)
import Helper.TH (input)
import Helper.Util (eol, number, parseWith, toTuple2)
import Text.ParserCombinators.Parsec (GenParser, char, eof, many1, sepBy1, string)

data Fold = FoldX Int | FoldY Int deriving (Eq, Ord, Show)

parser :: GenParser Char () (Set (Int, Int), [Fold])
parser = do
  let fX = string "x=" *> (FoldX <$> number)
      fY = string "y=" *> (FoldY <$> number)
  dots <- many1 (toTuple2 <$> number `sepBy1` char ',' <* eol) <* eol
  folds <- many1 ((string "fold along " >> (fX <|> fY)) <* eol) <* eof
  return (S.fromList dots, folds)

doFold :: Set (Int, Int) -> Fold -> Set (Int, Int)
doFold ps (FoldX c) = S.map (\(x, y) -> if x > c then (c * 2 - x, y) else (x, y)) ps
doFold ps (FoldY c) = S.map (\(x, y) -> if y > c then (x, c * 2 - y) else (x, y)) ps

part1 :: Int
part1 = $(input 13) & parseWith parser & second (!! 0) & uncurry doFold & S.size

part2 :: Text
part2 = $(input 13) & parseWith parser & uncurry (foldl' doFold) & fromCoords Wall & pretty
