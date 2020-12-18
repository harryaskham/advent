module TwentyTwenty.Day18 where

import Data.Char (digitToInt)
import Data.List (intercalate)
import Data.List.Utils (replace)
import Text.ParserCombinators.Parsec
  ( GenParser,
    between,
    char,
    choice,
    digit,
    eof,
    many,
    oneOf,
    sepBy,
    spaces,
    try,
    (<|>),
  )
import Util (eval, readWithParser)

inputPath :: String
inputPath = "input/2020/18.txt"

operation :: GenParser Char () (Int -> Int -> Int)
operation = do
  op <- between spaces spaces (oneOf "+*")
  return $ case op of
    '+' -> (+)
    '*' -> (*)

-- Parens flipped because of reversed input
parenExpression :: GenParser Char () Int
parenExpression = between (char ')') (char '(') expression

expression :: GenParser Char () Int
expression = do
  x <- parenExpression <|> digitToInt <$> digit
  try (operation <*> pure x <*> expression) <|> return x

parseEquations :: GenParser Char () [Int]
parseEquations = do
  answers <- many $ do
    answer <- expression
    char '\n'
    return answer
  eof
  return answers

part1 :: IO Int
part1 = do
  reversedInput <-
    (++ "\n") . intercalate "\n" . fmap reverse . lines
      <$> readFile inputPath
  return . sum $ readWithParser parseEquations reversedInput

reverseFixity :: String -> String
reverseFixity = replace "+" "+:" . replace "*" "*:"

part2 :: IO Integer
part2 = do
  equations <- fmap reverseFixity . lines <$> readFile inputPath
  sum <$> eval equations
