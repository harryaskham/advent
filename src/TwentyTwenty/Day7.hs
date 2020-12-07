module TwentyTwenty.Day7 where

import Data.Char (digitToInt, isDigit)
import Data.List (foldl')
import qualified Data.Map.Strict as M
import qualified Data.Sequence as SQ
import qualified Data.Set as S
import Text.ParserCombinators.Parsec
  ( GenParser,
    char,
    choice,
    count,
    eof,
    letter,
    many,
    many1,
    parse,
    satisfy,
    string,
    try,
    (<|>),
  )

inputPath :: String
inputPath = "input/2020/7.txt"

type Color = String

type Quantity = Int

data Rule = Rule Color [(Quantity, Color)] deriving (Show)

parseRules :: GenParser Char st [Rule]
parseRules = do
  rules <- many line
  eof
  return rules
  where
    line :: GenParser Char st Rule
    line = do
      color' <- color
      string "bags contain "
      contains <- noBags <|> many innerBag
      char '\n'
      return $ Rule (concat color') contains
    color :: GenParser Char st [Color]
    color = count 2 $ do
      c <- many1 letter
      string " "
      return c
    innerBag :: GenParser Char st (Quantity, Color)
    innerBag = do
      quantity <- satisfy isDigit
      string " "
      color' <- color
      string "bag"
      choice
        [ try (string ", "),
          try (string "."),
          try (string "s, "),
          try (string "s.")
        ]
      return (digitToInt quantity, concat color')
    noBags :: GenParser Char st [(Quantity, Color)]
    noBags = do
      string "no other bags."
      return []

type ContainedMap = M.Map Color [Color]

buildContainedMap :: [Rule] -> ContainedMap
buildContainedMap rules =
  M.fromListWith (++) $ concatMap flipRule rules
  where
    flipRule (Rule color contains) = (\(_, c) -> (c, [color])) <$> contains

containedWithin :: ContainedMap -> Color -> [Color]
containedWithin cm color = go cm S.empty (SQ.singleton color)
  where
    go cm seen queue =
      case SQ.viewl queue of
        SQ.EmptyL -> S.toList seen
        (color SQ.:< rest) -> case M.lookup color cm of
          Nothing -> go cm seen rest
          Just cs ->
            go
              cm
              (foldl' (flip S.insert) seen cs)
              (rest SQ.>< SQ.fromList cs)

readRules :: IO [Rule]
readRules = do
  f <- readFile inputPath
  case parse parseRules "(unknown)" f of
    Left e -> do
      print e
      return []
    Right rules -> return rules

part1 :: IO Int
part1 = do
  rules <- readRules
  return $ length $ containedWithin (buildContainedMap rules) "shinygold"

type RuleMap = M.Map Color [(Quantity, Color)]

buildRuleMap :: [Rule] -> RuleMap
buildRuleMap rules =
  M.fromList $
    (\(Rule color contains) -> (color, contains)) <$> rules

countBagsInside :: RuleMap -> Color -> Int
countBagsInside rm color =
  case M.lookup color rm of
    Nothing -> 0
    Just [] -> 0
    Just contains ->
      sum
        ( ( \(quantity, color') ->
              quantity + quantity * countBagsInside rm color'
          )
            <$> contains
        )

part2 :: IO Int
part2 = do
  rules <- readRules
  return $ countBagsInside (buildRuleMap rules) "shinygold"
