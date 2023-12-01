module Day1 (part1, part2) where

import Replace.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.Char
import Prelude hiding ((<|>), many, optional)
import Relude.Unsafe qualified as U
import Data.Text qualified as T
import Data.Text.Read
import Helper.TH
import Helper.Util

digit :: Parsec Void String Int
digit = U.read . pure <$> digitChar

word :: Parsec Void String Int
word = choice (try <$> [
                    string "one" $> 1,
                    string "two" $> 2,
                    string "three" $> 3,
                    string "four" $> 4,
                    string "five" $> 5,
                    string "six" $> 6,
                    string "seven" $> 7,
                    string "eight" $> 8,
                    string "nine" $> 9])

drow :: Parsec Void String Int
drow = choice (try <$> [
                    string "eno" $> 1,
                    string "owt" $> 2,
                    string "eerht" $> 3,
                    string "ruof" $> 4,
                    string "evif" $> 5,
                    string "xis" $> 6,
                    string "neves" $> 7,
                    string "thgie" $> 8,
                    string "enin" $> 9])

parser :: Parsec Void String Int -> Parsec Void String Int -> String -> Int
parser p q line =
  let go pq = snd . partitionEithers . unjust . parseMaybe (sepCap pq)
  in 10*U.head (go p line) + U.head (go q (reverse line))

solve :: Parsec Void String Int -> Parsec Void String Int -> Int
solve p q =
  $(input 1)
    & T.lines
    & fmap (parser p q . T.unpack)
    & sum

part1 :: Int
part1 = solve digit digit

part2 :: Int
part2 = solve  (digit <|> word) (digit <|> drow)
