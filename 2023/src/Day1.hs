module Day1 (part1, part2) where

import Replace.Megaparsec (sepCap)
import Text.Megaparsec (MonadParsec (try), choice, (<|>))
import Text.Megaparsec.Char (digitChar, string)
import Prelude hiding (choice, digit, many, optional, string, try, (<|>))

numbers :: [String]
numbers = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

digit :: MParser ℤ'
digit = uread . pure <$> digitChar

word :: (String -> String) -> MParser ℤ'
word f = choice (try <$> [string (f n) $> i | (n, i) <- zip numbers [1 ..]])

parser :: MParser ℤ' -> MParser ℤ' -> String -> ℤ'
parser p q line =
  let go pq = rights . parserM (sepCap pq)
   in 10 * uhead (go p line) + uhead (go q (reverse line))

solve :: MParser ℤ' -> MParser ℤ' -> ℤ'
solve p q = $(input 1) & lines & fmap (parser p q . unpack) & sum

part1 :: ℤ'
part1 = solve digit digit

part2 :: ℤ'
part2 = solve (digit <|> word id) (digit <|> word reverse)
