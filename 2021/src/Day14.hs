module Day14 (part1, part2) where

import Data.List (maximum, minimum, (!!))
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Helper.TH (input)
import Helper.Util (countMap, eol, pairs, parseWith, toTuple2)
import Text.ParserCombinators.Parsec (GenParser, eof, letter, many1, sepBy, string)

parser :: GenParser Char () (Map String Char, (Char, Char, Map (Char, Char) Int))
parser = do
  template <- many1 letter <* (eol >> eol)
  rules <- many1 (toTuple2 <$> (many1 letter `sepBy` string " -> ") <* eol) <* eof
  return
    ( M.fromList (second (!! 0) <$> rules),
      (L.head template, L.last template, countMap . pairs $ template)
    )

applyRules :: Map String Char -> Map (Char, Char) Int -> Map (Char, Char) Int
applyRules rules pairCounts =
  M.fromListWith
    (+)
    ( concat
        [ [((a, c), v), ((c, b), v)]
          | ((a, b), v) <- M.toList pairCounts,
            let c = rules M.! [a, b]
        ]
    )

occurrences :: Char -> Char -> Map (Char, Char) Int -> Map Char Int
occurrences fc lc pairCounts =
  M.adjust (+ 1) fc
    . M.adjust (+ 1) lc
    . fmap (`div` 2)
    $ M.fromListWith
      (+)
      ( concat
          [ [(a, v), (b, v)]
            | ((a, b), v) <- M.toList pairCounts
          ]
      )

solve :: Int -> Int
solve n =
  let (rules, (fc, lc, pairCounts)) = parseWith parser $(input 14)
   in iterate (applyRules rules) pairCounts
        & (!! n)
        & occurrences fc lc
        & M.elems
        & ((-) <$> maximum <*> minimum)

part1 :: Int
part1 = solve 10

part2 :: Int
part2 = solve 40
