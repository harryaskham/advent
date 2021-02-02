module TwentySixteen.Day15 where

import Data.Char (digitToInt)
import Data.List (nub)
import Text.ParserCombinators.Parsec
  ( GenParser,
    digit,
    eof,
    many1,
    string,
  )
import Util (eol, input, readWithParser)

data Disc = Disc Int Int Int

discs :: GenParser Char () [Disc]
discs = do
  ds <- many1 disc
  eof
  return ds
  where
    disc = do
      string "Disc #"
      dId <- digitToInt <$> digit
      string " has "
      positions <- read <$> many1 digit
      string " positions; at time=0, it is at position "
      position <- digitToInt <$> digit
      string "."
      eol
      return $ Disc dId positions position

configuration :: [Disc] -> Int -> [Int]
configuration ds t =
  [ (position + dId + t) `mod` positions
    | (Disc dId positions position) <- ds
  ]

solve :: [Disc] -> Int
solve ds = head $ filter ((== [0]) . nub . configuration ds) [0 ..]

part1 :: IO Int
part1 = do
  ds <- readWithParser discs <$> input 2016 15
  return . solve $ ds

part2 :: IO Int
part2 = do
  ds <- readWithParser discs <$> input 2016 15
  let ds' = ds ++ [Disc (length ds + 1) 11 0]
  return . solve $ ds'
