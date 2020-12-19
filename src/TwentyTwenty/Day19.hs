module TwentyTwenty.Day19 where

import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Text.ParserCombinators.Parsec
  ( GenParser,
    anyToken,
    between,
    char,
    choice,
    digit,
    eof,
    many,
    many1,
    oneOf,
    string,
    try,
  )
import Util (readWithParser, (<$$>))

inputPath :: String
inputPath = "input/2020/19.txt"

type RuleId = Int

data Rule = Match Char | Alternatives [[RuleId]] deriving (Show)

type Message = String

parseRule :: GenParser Char () (RuleId, Rule)
parseRule = do
  rId <- many1 digit
  string ": "
  r <-
    choice
      [ try $ do
          match <- between (char '"') (char '"') (oneOf "ab")
          return (Match match),
        try $ do
          altString <- many anyToken
          return $ Alternatives (read <$$> (splitOn " " <$> splitOn " | " altString))
      ]
  eof
  return (read rId, r)

matches :: M.Map RuleId Rule -> RuleId -> [String]
matches ruleMap ruleId =
  case M.lookup ruleId ruleMap of
    Just (Match c) -> [[c]]
    Just (Alternatives alts) -> concat $ handleAlt <$> alts
  where
    handleAlt alt = concat <$> sequence (matches ruleMap <$> alt)

readInput :: IO (M.Map RuleId Rule, [Message])
readInput = do
  ls <- lines <$> readFile inputPath
  let [rulesLs, messages] = splitOn [""] ls
      rules = M.fromList $ readWithParser parseRule <$> rulesLs
  return (rules, messages)

part1 :: IO Int
part1 = do
  (rules, messages) <- readInput
  let good = S.fromList $ matches rules 0
  return . length $ filter (`S.member` good) messages

validateMessage :: M.Map RuleId Rule -> GenParser Char () Bool
validateMessage rules =
  choice
    [ try $ do
        a <- many1 (choice (try . string <$> matches rules 42))
        b <- many1 (choice (try . string <$> matches rules 31))
        eof
        return $ length a > length b,
      return False
    ]

part2 :: IO Int
part2 = do
  (rules, messages) <- readInput
  return . length . filter (readWithParser (validateMessage rules)) $ messages
