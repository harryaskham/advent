module TwentyFifteen.Day16 where

import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (isNothing)
import Text.ParserCombinators.Parsec
  ( GenParser,
    digit,
    eof,
    letter,
    many,
    many1,
    sepBy,
    string,
  )
import Util (eol, input, number, readWithParser)

sues :: GenParser Char () [Map String Int]
sues = many sue <* eof
  where
    sue = M.fromList <$> (string "Sue " >> many1 digit >> string ": " *> (kv `sepBy` string ", ") <* eol)
    kv = (,) <$> many1 letter <* string ": " <*> number

requirements :: Map String Int
requirements =
  M.fromList
    [ ("children", 3),
      ("cats", 7),
      ("samoyeds", 2),
      ("pomeranians", 3),
      ("akitas", 0),
      ("vizslas", 0),
      ("goldfish", 5),
      ("trees", 3),
      ("cars", 2),
      ("perfumes", 1)
    ]

possibleSue :: Map String Int -> Bool
possibleSue sue = all (== True) [v == requirements M.! k | (k, v) <- M.toList sue]

solve :: (Map String Int -> Bool) -> IO Int
solve f = do
  ss <- readWithParser sues <$> input 2015 16
  return . fst . head $ filter (f . snd) (zip [1 ..] ss)

part1 :: IO Int
part1 = solve possibleSue

possibleSue2 :: Map String Int -> Bool
possibleSue2 sue = eqs && lts && gts
  where
    eqs =
      all
        (== True)
        [ v == requirements M.! k
          | (k, v) <- M.toList sue,
            not (k `elem` ["cats", "trees", "pomeranians", "goldfish"])
        ]
    gts =
      all
        (== True)
        [ isNothing v || v > M.lookup k requirements
          | k <- ["cats", "trees"],
            let v = M.lookup k sue
        ]
    lts =
      all
        (== True)
        [ isNothing v || v < M.lookup k requirements
          | k <- ["pomeranians", "goldfish"],
            let v = M.lookup k sue
        ]

part2 :: IO Int
part2 = solve possibleSue2
