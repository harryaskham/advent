module Day8 (part1, part2) where

import Data.Bimap (Bimap)
import Data.Bimap qualified as BM
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Helper.TH (input)
import Helper.Util (applyPermutationMap, count, listAsInt, parseLinesWith, permutationMaps, toTuple2, unlist, (<$$>))
import Text.ParserCombinators.Parsec (GenParser, char, letter, many1, sepBy1, string)

data Segment = A | B | C | D | E | F | G deriving (Eq, Ord, Enum)

fromChar :: Char -> Segment
fromChar = (M.fromList (zip ['a' .. 'g'] [A .. G]) M.!)

digits :: Bimap Int (Set Segment)
digits =
  BM.mapR S.fromList . BM.fromList $
    [ (0, [A, B, C, E, F, G]),
      (1, [C, F]),
      (2, [A, C, D, E, G]),
      (3, [A, C, D, F, G]),
      (4, [B, C, D, F]),
      (5, [A, B, D, F, G]),
      (6, [A, B, D, E, F, G]),
      (7, [A, C, F]),
      (8, [A, B, C, D, E, F, G]),
      (9, [A, B, C, D, F, G])
    ]

validPermutation :: [Set Segment] -> Map Segment Segment
validPermutation ss =
  unlist
    [ p
      | p <- permutationMaps,
        all (\s -> applyPermutationMap p s `BM.memberR` digits) ss
    ]

line :: GenParser Char () [Int]
line = do
  let segment = S.fromList <$> (fromChar <$$> many1 letter)
      segments = many1 (segment <* optional (char ' '))
  p <- validPermutation <$> (segments <* string "| ")
  (digits BM.!>) . applyPermutationMap p <$$> segments

part1 :: Int
part1 = parseLinesWith line $(input 8) & fmap (count (`elem` [1, 4, 7, 8])) & sum

part2 :: Int
part2 = parseLinesWith line $(input 8) & fmap listAsInt & sum
