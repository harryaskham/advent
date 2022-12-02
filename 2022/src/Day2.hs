module Day2 (part1, part2) where

import Helper.TH (input)
import Helper.Util (eol, parseWith, predCyc, succCyc)
import Text.ParserCombinators.Parsec (Parser, char, eof, many1, spaces, (<|>))
import Prelude hiding ((<|>))

data RPS = Rock | Paper | Scissors deriving (Eq, Enum, Bounded)

data Entry = Entry RPS RPS

parser :: Parser [Entry]
parser = many1 (entry <* eol) <* eof
  where
    rps = rock <|> paper <|> scissors
    rock = (char 'A' <|> char 'X') $> Rock
    paper = (char 'B' <|> char 'Y') $> Paper
    scissors = (char 'C' <|> char 'Z') $> Scissors
    entry = Entry <$> rps <* spaces <*> rps

value :: RPS -> Integer
value Rock = 1
value Paper = 2
value Scissors = 3

outcomeScore :: RPS -> RPS -> Integer
outcomeScore a b
  | a == succCyc b = 0
  | a == b = 3
  | otherwise = 6

scoreEntry :: Entry -> Integer
scoreEntry (Entry a b) = outcomeScore a b + value b

updateEntry :: Entry -> Entry
updateEntry (Entry a Rock) = Entry a (predCyc a)
updateEntry (Entry a Paper) = Entry a a
updateEntry (Entry a Scissors) = Entry a (succCyc a)

solve :: (Entry -> Entry) -> Integer
solve f = $(input 2) & parseWith parser & fmap (scoreEntry . f) & sum

part1 :: Integer
part1 = solve id

part2 :: Integer
part2 = solve updateEntry
