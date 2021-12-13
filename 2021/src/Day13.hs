module Day13 (part1, part2) where

import Control.Arrow (app)
import Data.List ((!!))
import Data.Set qualified as S
import Helper.Grid (SimpleWall (Wall), fromCoords, pretty)
import Helper.TH (input)
import Helper.Util (eol, number, parseWith, toTuple2)
import Text.ParserCombinators.Parsec (GenParser, char, eof, many1, sepBy1, string)

parser :: GenParser Char () ([Set (Int, Int) -> Set (Int, Int)], Set (Int, Int))
parser =
  let f' e m c = S.map (\p -> if e p > c then m ((+ c * 2) . negate) p else p)
      f ch e m = string (ch : "=") *> (f' e m <$> number)
   in flip (,)
        <$> (S.fromList <$> (many1 (toTuple2 <$> number `sepBy1` char ',' <* eol) <* eol))
        <*> (many1 ((string "fold along " >> (f 'x' fst first <|> f 'y' snd second)) <* eol) <* eof)

part1 :: Int
part1 = $(input 13) & parseWith parser & first (!! 0) & app & S.size

part2 :: Text
part2 = $(input 13) & parseWith parser & uncurry (flip (foldl' (&))) & fromCoords Wall & pretty
