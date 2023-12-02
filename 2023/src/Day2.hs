module Day2 (part1, part2) where

import Data.Array qualified as A
import Data.Bimap (Bimap)
import Data.Bimap qualified as BM
import Data.Map.Strict qualified as M
import Data.Mod
import Data.PQueue.Prio.Min qualified as PQ
import Data.Sequence qualified as SQ
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Read
import Data.Vector qualified as V
import Helper.Coord
import Helper.Grid
import Helper.TH
import Helper.Tracers
import Helper.Util
import Text.ParserCombinators.Parsec

-- parser :: Parser [Int]
-- parser = many1 (number <* eol) <* eof

-- line :: Parser Int
-- line = number

-- data Cell
--   = Empty
--   | Wall
--   deriving (Eq, Ord)

-- instance GridCell Cell where
--   charMap =
--     BM.fromList
--       [ (Empty, ' '),
--         (Wall, '#')
--       ]

data Color = Red Int | Blue Int | Green Int
  deriving (Show)

data Game = Game
  { index :: Int,
    cubes :: [[Color]]
  }
  deriving (Show)

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

part1 :: Int
part1 =
  $(input 2)
    & parseWith games
    & filter possibleGame
    & fmap index
    & sum

power :: [Color] -> Int
power colors = go colors 0 0 0
  where
    go [] r g b = r * g * b
    go ((Red i) : cs) r g b = go cs (max i r) g b
    go ((Green i) : cs) r g b = go cs r (max i g) b
    go ((Blue i) : cs) r g b = go cs r g (max i b)

part2 :: Int
part2 =
  $(input 2)
    & parseWith games
    & fmap (power . mconcat . cubes)
    & sum
