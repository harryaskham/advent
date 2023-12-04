module Day4 (part1, part2) where

import Data.List (intersect)
import Data.Map.Strict qualified as M
import Helper.TH (input)
import Helper.Util (eol, number, parseWith)
import Text.ParserCombinators.Parsec (Parser, eof, many, many1, sepBy1, string)
import Prelude hiding (many)

parser :: Parser [(Int, [Int], [Int])]
parser = many1 (line <* eol) <* eof
  where
    gap = many $ string " "
    line = do
      i <- (string "Card" >> gap) *> number <* (string ":" >> gap)
      as <- many1 (number <* gap) <* (string "|" >> gap)
      bs <- number `sepBy1` gap
      return (i, as, bs)

part1 :: Int
part1 =
  $(input 4)
    & parseWith parser
    & fmap (\(_, as, bs) -> 2 ^ length (as `intersect` bs) `div` 2)
    & sum

part2 :: Int
part2 =
  $(input 4)
    & parseWith parser
    & ( \cards ->
          foldl'
            ( \m (i, as, bs) ->
                let n = length (as `intersect` bs)
                    c = fromMaybe 0 $ M.lookup i m
                 in foldl' (flip (M.adjust (+ c))) m [i + 1 .. i + n]
            )
            (M.fromList [(i, 1) | (i, _, _) <- cards])
            cards
      )
    & M.toList
    & fmap snd
    & sum
