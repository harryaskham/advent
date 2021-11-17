{-# LANGUAGE TupleSections #-}

module TwentySeventeen.Day7 where

import qualified Data.Map.Strict as M
import Text.ParserCombinators.Parsec
  ( GenParser,
    char,
    digit,
    eof,
    letter,
    many1,
    sepBy,
    string,
    try,
    (<|>),
  )
import Util (readWithParser)

inputPath :: String
inputPath = "input/2017/7.txt"

parseTree :: Ord k => (String -> Int -> [String] -> [(k, v)]) -> GenParser Char () (M.Map k v)
parseTree mkEntry = do
  entries <- many1 line
  eof
  return $ M.fromList $ concat entries
  where
    line = do
      name <- many1 letter
      string " ("
      weight <- read <$> many1 digit
      string ")"
      children <-
        try
          ( do
              string " -> "
              many1 letter `sepBy` string ", "
          )
          <|> return []
      char '\n'
      return $ mkEntry name weight children

findRoot :: String -> M.Map String String -> String
findRoot node tree =
  case M.lookup node tree of
    Nothing -> node
    Just parent -> findRoot parent tree

childToParent :: String -> Int -> [String] -> [(String, String)]
childToParent name _ children = (,name) <$> children

parentToChild :: String -> Int -> [String] -> [(String, (Int, [String]))]
parentToChild name weight children = [(name, (weight, children))]

part1 :: IO String
part1 = do
  tree <- readWithParser (parseTree childToParent) <$> readFile inputPath
  let node = head . M.keys $ tree
  return $ findRoot node tree

weighFrom :: String -> M.Map String (Int, [String]) -> Int
weighFrom node tree = weight + sum (weighFrom <$> children <*> pure tree)
  where
    (weight, children) = tree M.! node

findImbalance :: String -> M.Map String (Int, [String]) -> Int -> Int
findImbalance current tree targetWeight
  | M.size childWeightCounts == 1 = targetWeight - childWeightSum
  | null children = targetWeight
  | otherwise = findImbalance oddChild tree goodWeight
  where
    (_, children) = tree M.! current
    childWeights = [(weighFrom child tree, [child]) | child <- children]
    childWeightCounts = M.fromListWith (++) childWeights
    oddChild = head . head . M.elems . M.filter ((== 1) . length) $ childWeightCounts
    goodWeight = head . M.keys . M.filter ((> 1) . length) $ childWeightCounts
    childWeightSum = ((*) <$> fst <*> (length . snd)) $ head $ M.toList childWeightCounts

part2 :: IO Int
part2 = do
  rootNode <- part1
  tree <- readWithParser (parseTree parentToChild) <$> readFile inputPath
  return $ findImbalance rootNode tree 0
