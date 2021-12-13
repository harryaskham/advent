module Day13 (part1, part2) where

import Data.List.NonEmpty qualified as NE
import Data.Set qualified as S
import Helper.Grid (SimpleWall (Wall), fromCoords, pretty)
import Helper.TH (input)
import Helper.Util (appWhen, eol, number, parseWith, toTuple2)
import Text.ParserCombinators.Parsec (GenParser, char, eof, many1, sepBy1, string)

parser :: GenParser Char () (NonEmpty (Set (Int, Int)))
parser = do
  let f' e m c = S.map (appWhen ((> c) . e) (m ((+ c * 2) . negate)))
      f ch e m = string (ch : "=") *> (f' e m <$> number)
  dots <- S.fromList <$> (many1 (toTuple2 <$> number `sepBy1` char ',' <* eol) <* eol)
  folds <- many1 ((string "fold along " >> (f 'x' fst first <|> f 'y' snd second)) <* eol) <* eof
  return $ NE.fromList (scanl' (&) dots folds)

part1 :: Int
part1 = $(input 13) & parseWith parser & (NE.!! 1) & S.size

part2 :: Text
part2 = $(input 13) & parseWith parser & last & fromCoords Wall & pretty
