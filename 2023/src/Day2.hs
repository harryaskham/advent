module Day2 (part1, part2) where

import Data.Text qualified as T
import Helper.TH (input)
import Helper.Util (eol, number, parseWith)
import Text.ParserCombinators.Parsec
  ( Parser,
    choice,
    eof,
    sepBy1,
    string,
  )

data Color = Red Int | Blue Int | Green Int
  deriving (Show)

data Game = Game
  { index :: Int,
    cubes :: [[Color]]
  }

color :: Parser Color
color = do
  amount <- number
  string " "
  c <- choice [string "red", string "green", string "blue"]
  return case c of
    "red" -> Red amount
    "green" -> Green amount
    "blue" -> Blue amount

game :: Parser Game
game = do
  string "Game "
  index <- number
  string ": "
  cubes <- (color `sepBy1` string ", ") `sepBy1` string "; "
  return $ Game index cubes

games :: Parser [Game]
games = game `sepBy1` eol <* eof

possibleColor :: Color -> Bool
possibleColor (Red i) = i <= 12
possibleColor (Green i) = i <= 13
possibleColor (Blue i) = i <= 14

possibleGame :: Game -> Bool
possibleGame (Game _ colors) = all (all possibleColor) colors

power :: [Color] -> Int
power colors = go colors 0 0 0
  where
    go [] r g b = r * g * b
    go ((Red i) : cs) r g b = go cs (max i r) g b
    go ((Green i) : cs) r g b = go cs r (max i g) b
    go ((Blue i) : cs) r g b = go cs r g (max i b)

part1 :: Int
part1 =
  $(input 2)
    & parseWith games
    & filter possibleGame
    & fmap index
    & sum

part2 :: Int
part2 =
  $(input 2)
    & parseWith games
    & fmap (power . mconcat . cubes)
    & sum
