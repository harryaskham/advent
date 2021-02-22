module TwentyFifteen.Day15 where

import Data.List (transpose)
import Text.ParserCombinators.Parsec
  ( GenParser,
    eof,
    letter,
    many,
    many1,
    oneOf,
    sepBy,
    string,
  )
import Util (eol, input, number, readWithParser)

type Ingredient = (String, Int, Int, Int, Int, Int)

ingredients :: GenParser Char () [Ingredient]
ingredients = many line <* eof
  where
    line = do
      name <- many1 letter
      string ": capacity "
      [cap, dur, fla, tex, cal] <-
        number `sepBy` (many (oneOf "abcdefghijklmnopqrstuvwxyz, "))
      eol
      return (name, cap, dur, fla, tex, cal)

score :: [Ingredient] -> [Int] -> Int
score is spoons =
  product
    . fmap (\x -> if x < 0 then 0 else x)
    . fmap sum
    . transpose
    $ [ [n * cap, n * dur, n * fla, n * tex]
        | (n, (_, cap, dur, fla, tex, _)) <- zip spoons is
      ]

solve :: ([Ingredient] -> [Int] -> Int) -> IO Int
solve f = do
  is <- readWithParser ingredients <$> input 2015 15
  return $
    maximum
      [ f is [a, b, c, d]
        | a <- [0 .. 100],
          b <- [0 .. 100 - a],
          c <- [0 .. 100 - a - b],
          let d = 100 - a - b - c
      ]

part1 :: IO Int
part1 = solve score

score2 :: [Ingredient] -> [Int] -> Int
score2 is spoons
  | cals == 500 = score
  | otherwise = 0
  where
    propsCals =
      [ ([n * cap, n * dur, n * fla, n * tex], n * cal)
        | (n, (_, cap, dur, fla, tex, cal)) <- zip spoons is
      ]
    score =
      product
        . fmap (\x -> if x < 0 then 0 else x)
        . fmap sum
        . transpose
        $ fst <$> propsCals
    cals = sum (snd <$> propsCals)

part2 :: IO Int
part2 = solve score2
