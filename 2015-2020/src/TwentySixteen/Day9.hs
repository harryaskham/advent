module TwentySixteen.Day9 where

import Data.List.Extra (breakOn)
import Text.ParserCombinators.Parsec
  ( GenParser,
    anyChar,
    between,
    char,
    count,
    digit,
    eof,
    many,
    many1,
    noneOf,
  )
import Util (readWithParser)

inputPath :: String
inputPath = "input/2016/9.txt"

compression :: GenParser Char () (Int, Int)
compression = between (char '(') (char ')') $ do
  n <- many1 digit
  char 'x'
  r <- many1 digit
  return (read n, read r)

parseInput :: GenParser Char () String
parseInput = do
  clauses <- many1 clause
  eof
  return $ concat clauses
  where
    clause = do
      inits <- many (noneOf "(")
      (n, r) <- compression
      next <- count n anyChar
      rest <- many (noneOf "(")
      return $ inits ++ concat (replicate r next) ++ rest

part1 :: IO Int
part1 = length . readWithParser parseInput . head . lines <$> readFile inputPath

splitCompression :: String -> ((Int, Int), String)
splitCompression = readWithParser $ do
  (n, r) <- compression
  rest <- many anyChar
  eof
  return ((n, r), rest)

getLen :: String -> Int
getLen "" = 0
getLen s@('(' : _) =
  let ((n, r), rest) = splitCompression s
   in r * getLen (take n rest) + getLen (drop n rest)
getLen s = let (x, xs) = breakOn "(" s in length x + getLen xs

part2 :: IO Int
part2 = getLen . head . lines <$> readFile inputPath
