module Day5 (part1, part2) where

import Data.Vector (Vector)
import Data.Vector qualified as V
import Helper.TH (input)
import Helper.Util (eol, numberLine3, parseWith, skipLine)
import Relude.Unsafe qualified as U
import Text.ParserCombinators.Parsec (Parser, anyChar, between, char, count, eof, many1, sepBy, string, (<|>))
import Prelude hiding ((<|>))

parser :: Parser (Vector [Char], [(Int, Int, Int)])
parser = (,) <$> (stackRows <* (skipLine >> skipLine)) <*> (many1 numberLine3 <* eof)
  where
    stackRows = V.fromList . fmap catMaybes . transpose <$> count 8 stackRow
    stackRow = (stackItem `sepBy` char ' ') <* eol
    stackItem = emptyStackItem <|> fullStackItem
    emptyStackItem = string "   " $> Nothing
    fullStackItem = Just <$> between (char '[') (char ']') anyChar

runMove :: ([Char] -> [Char]) -> Vector [Char] -> (Int, Int, Int) -> Vector [Char]
runMove revFn stacks (n, s, d) =
  stacks V.// [(s', drop n $ stacks V.! s'), (d', toMove ++ stacks V.! d')]
  where
    (s', d') = (s - 1, d - 1)
    toMove = revFn . take n $ stacks V.! s'

solve :: ([Char] -> [Char]) -> String
solve revFn =
  $(input 5)
    & parseWith parser
    & uncurry (foldl' (runMove revFn))
    & fmap U.head
    & V.toList

part1 :: String
part1 = solve reverse

part2 :: String
part2 = solve id
