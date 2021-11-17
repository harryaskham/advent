module TwentySeventeen.Day9 where

import Text.ParserCombinators.Parsec
  ( GenParser,
    anyChar,
    between,
    char,
    many,
    noneOf,
    sepBy,
    try,
    (<|>),
  )
import Util (readWithParser)

inputPath :: String
inputPath = "input/2017/9.txt"

groupCounts :: Int -> GenParser Char () Int
groupCounts depth = try group <|> try garbage
  where
    group = (+ depth) . sum <$> between (char '{') (char '}') (groupCounts (depth + 1) `sepBy` char ',')
    garbage = between (char '<') (char '>') (many garbageAtom) >> return 0
    garbageAtom = try (char '!' >> anyChar) <|> noneOf ">"

solve :: GenParser Char () Int -> IO Int
solve parser = do
  stream <- head . lines <$> readFile inputPath
  return $ readWithParser parser stream

part1 :: IO Int
part1 = solve (groupCounts 1)

garbageCounts :: GenParser Char () Int
garbageCounts = try group <|> try garbage
  where
    group = sum <$> between (char '{') (char '}') (garbageCounts `sepBy` char ',')
    garbage = sum <$> between (char '<') (char '>') (many garbageAtom)
    garbageAtom = try (char '!' >> anyChar >> return 0) <|> (noneOf ">" >> return 1)

part2 :: IO Int
part2 = solve garbageCounts
