module TwentyTwenty.Day24 where

import Data.List (foldl')
import Data.Maybe (catMaybes)
import qualified Data.Set as S
import Text.ParserCombinators.Parsec
  ( GenParser,
    char,
    choice,
    eof,
    many1,
    string,
    try,
  )
import Util (readWithParser)

inputPath :: String
inputPath = "input/2020/24.txt"

data HDir = E | W | NE | NW | SE | SW deriving (Show)

fromString :: String -> HDir
fromString "e" = E
fromString "w" = W
fromString "ne" = NE
fromString "nw" = NW
fromString "se" = SE
fromString "sw" = SW

tiles :: GenParser Char () [[HDir]]
tiles = do
  lines <- many1 line
  eof
  return lines
  where
    line = do
      ss <-
        many1 $
          choice
            [ try (string "e"),
              try (string "w"),
              try (string "ne"),
              try (string "nw"),
              try (string "se"),
              try (string "sw")
            ]
      char '\n'
      return $ fromString <$> ss

data HexC = HexC Int Int deriving (Show, Eq, Ord)

move :: HexC -> HDir -> HexC
move (HexC x y) E = HexC (x + 2) y
move (HexC x y) W = HexC (x - 2) y
move (HexC x y) NE = HexC (x + 1) (y + 1)
move (HexC x y) NW = HexC (x - 1) (y + 1)
move (HexC x y) SE = HexC (x + 1) (y - 1)
move (HexC x y) SW = HexC (x - 1) (y - 1)

flipTiles :: [[HDir]] -> S.Set HexC -> S.Set HexC
flipTiles [] blacks = blacks
flipTiles (ts : tss) blacks = flipTiles tss nextBlacks
  where
    tile = foldl' move (HexC 0 0) ts
    nextBlacks =
      if tile `S.member` blacks
        then S.delete tile blacks
        else S.insert tile blacks

part1 :: IO Int
part1 = do
  ts <- readWithParser tiles <$> readFile inputPath
  return $ S.size (flipTiles ts S.empty)

neighbours :: HexC -> [HexC]
neighbours c = move c <$> [E, W, NE, NW, SE, SW]

step :: S.Set HexC -> S.Set HexC
step blacks = S.fromList $ catMaybes $ handleCoord <$> S.toList coords
  where
    coords = S.fromList (neighbours =<< S.toList blacks) `S.union` blacks
    handleCoord c =
      let bn = S.size $ S.fromList (neighbours c) `S.intersection` blacks
       in if c `S.member` blacks
            then
              if bn == 0 || bn > 2
                then Nothing
                else Just c
            else
              if bn == 2
                then Just c
                else Nothing

part2 :: IO Int
part2 = do
  ts <- readWithParser tiles <$> readFile inputPath
  let blacks = flipTiles ts S.empty
  return $ S.size (iterate step blacks !! 100)
