module Day4 (part1, part2) where

import Data.List (intersect)
import Data.Map.Strict qualified as M
import Helper.TH (input)
import Helper.Util (eol, number, parseWith)
import Text.ParserCombinators.Parsec
  ( Parser,
    count,
    eof,
    many,
    many1,
    sepBy1,
    string,
  )
import Prelude hiding (many, optional)

parser :: Parser [(Int, [Int], [Int])]
parser = many1 (line <* eol) <* eof
  where
    whitespace = many $ string " "
    line = do
      i <- (string "Card" >> whitespace) *> number <* (string ":" >> whitespace)
      as <- count 10 (number <* whitespace) <* (string "|" >> whitespace)
      bs <- number `sepBy1` whitespace
      return (i, as, bs)

part1 :: Int
part1 =
  $(input 4)
    & parseWith parser
    & fmap (\(_, as, bs) -> (2 ^ length (as `intersect` (bs :: [Int]))) `div` 2)
    & sum

part2 :: Int
part2 =
  $(input 4)
    & parseWith parser
    & ( \cards ->
          foldl'
            ( \m (i, as, bs) ->
                let n = length (as `intersect` (bs :: [Int]))
                    c = fromMaybe 0 $ M.lookup i m
                 in foldl' (flip (M.adjust (+ c))) m [i + 1 .. i + n]
            )
            (M.fromList [(i, 1) | (i, _, _) <- cards])
            cards
      )
    & M.toList
    & fmap snd
    & sum
