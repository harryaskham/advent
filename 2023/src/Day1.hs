module Day1 (part1, part2) where

import Data.Text qualified as T
import Helper.TH (input)
import Helper.Util (MParser, digit, parserM)
import Relude.Unsafe qualified as U
import Replace.Megaparsec (sepCap)
import Text.Megaparsec (MonadParsec (try), choice, (<|>))
import Text.Megaparsec.Char (string)
import Prelude hiding (many, optional, (<|>))

numbers :: [String]
numbers =
  [ "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "nine"
  ]

word :: (String -> String) -> MParser Int
word f =
  choice
    ( try
        <$> [ string (f n) $> i
              | (n, i) <- zip numbers [1 ..]
            ]
    )

parser :: MParser Int -> MParser Int -> String -> Int
parser p q line =
  let go pq = rights . parserM (sepCap pq)
   in 10 * U.head (go p line) + U.head (go q (reverse line))

solve :: MParser Int -> MParser Int -> Int
solve p q =
  $(input 1)
    & T.lines
    & fmap (parser p q . T.unpack)
    & sum

part1 :: Int
part1 = solve digit digit

part2 :: Int
part2 = solve (digit <|> word id) (digit <|> word reverse)
