module TwentyTwenty.Day18 where

import Data.Char (digitToInt)
import Data.List (intercalate)
import Data.List.Utils (replace)
import Text.ParserCombinators.Parsec
  ( GenParser,
    char,
    choice,
    digit,
    eof,
    many,
    oneOf,
    spaces,
    try,
    (<|>),
  )
import Util (eval, readWithParser)

inputPath :: String
inputPath = "input/2020/18.txt"

parseEquations :: GenParser Char () [Int]
parseEquations = do
  answers <- many $ do
    eqs <- equations
    char '\n'
    return eqs
  eof
  return answers
  where
    operation = do
      op <- oneOf "+*"
      case op of
        '+' -> return (+)
        '*' -> return (*)
    singleDigit = digitToInt <$> digit
    -- Parens flipped because of reversed input
    parenClause = do
      char ')'
      x <- equations
      char '('
      return x
    clause = parenClause <|> singleDigit
    equations = do
      x <- clause
      try (restOfEquation x) <|> return x
    restOfEquation x = do
      spaces
      op <- operation
      spaces
      rest <- equations
      return $ x `op` rest

part1 :: IO Int
part1 = do
  reversedInput <-
    (++ "\n")
      . intercalate "\n"
      . fmap reverse
      . lines
      <$> readFile inputPath
  return . sum $ readWithParser parseEquations reversedInput

reverseFixity :: String -> String
reverseFixity = replace "+" "+:" . replace "*" "*:"

part2 :: IO Integer
part2 = do
  equations <- fmap reverseFixity . lines <$> readFile inputPath
  sum <$> eval equations
