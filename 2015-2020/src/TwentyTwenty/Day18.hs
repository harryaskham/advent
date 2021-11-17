module TwentyTwenty.Day18 where

import Data.Char (digitToInt)
import Data.List.Utils (replace)
import Text.ParserCombinators.Parsec
  ( GenParser,
    between,
    char,
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

part1 :: IO Int
part1 = do
  expressions <- fmap reverse . lines <$> readFile inputPath
  return . sum $ readWithParser expression <$> expressions

reverseFixity :: String -> String
reverseFixity = replace "+" "+:" . replace "*" "*:"

part2 :: IO Integer
part2 = do
  expressions <- fmap reverseFixity . lines <$> readFile inputPath
  sum <$> eval expressions
