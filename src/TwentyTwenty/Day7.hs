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

-- Encode the color and the contained quantity of other colours
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

-- A map from color to all the colors that directly contain it
type ContainedMap = M.Map Color [Color]

buildContainedMap :: [Rule] -> ContainedMap
buildContainedMap rules =
  M.fromListWith (++) $ concatMap flipRule rules
  where
    flipRule (Rule color contains) = (\(_, c) -> (c, [color])) <$> contains

-- BFS from the starting color to find all containing colors
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

part1 :: IO Int
part1 = do
  ls <- lines <$> readFile inputPath
  let rules = fst <$> (concat $ readP_to_S parseRule <$> ls)
      containedMap = buildContainedMap rules
  return $ length $ containedWithin containedMap "shinygold"

-- A map from color to all the colors it contains
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
  ls <- lines <$> readFile inputPath
  let rules = fst <$> (concat $ readP_to_S parseRule <$> ls)
      ruleMap = buildRuleMap rules
  return $ countBagsInside ruleMap "shinygold"
