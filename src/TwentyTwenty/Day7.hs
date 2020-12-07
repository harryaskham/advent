module TwentyTwenty.Day7 where

import Data.Char (digitToInt, isDigit, isLetter)
import Data.List (foldl')
import qualified Data.Map.Strict as M
import qualified Data.Sequence as SQ
import qualified Data.Set as S
import Text.ParserCombinators.ReadP
  ( ReadP,
    count,
    eof,
    many,
    munch1,
    readP_to_S,
    satisfy,
    string,
    (<++),
  )

inputPath :: String
inputPath = "input/2020/7.txt"

type Color = String

type Quantity = Int

data Rule = Rule Color [(Quantity, Color)] deriving (Show)

parseRule :: ReadP Rule
parseRule = do
  color <- parseColor
  string "bags contain "
  contains <-
    ( do
        string "no other bags."
        return []
      )
      <++ ( many $ do
              quantity <- satisfy isDigit
              string " "
              color' <- parseColor
              string "bag, " <++ string "bag." <++ string "bags, " <++ string "bags."
              return (digitToInt quantity, concat color')
          )
  eof
  return $ Rule (concat color) contains
  where
    parseColor = count 2 $ do
      c <- munch1 isLetter
      string " "
      return c

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
  ls <- lines <$> readFile inputPath
  return $ fst <$> concatMap (readP_to_S parseRule) ls

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
