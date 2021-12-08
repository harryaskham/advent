module Day8 (part1, part2) where

import Data.Bimap (Bimap)
import Data.Bimap qualified as BM
import Data.List ((!!))
import Data.Set qualified as S
import Helper.TH (input)
import Helper.Util (applyPermutationMap, listAsInt, parseLinesWith, permutationMaps, toTuple2, unlist, (<$$>))
import Text.ParserCombinators.Parsec (GenParser, char, letter, many1, sepBy1, string)

data Segment = A | B | C | D | E | F | G deriving (Eq, Ord, Enum)

fromChar :: Char -> Segment
fromChar 'a' = A
fromChar 'b' = B
fromChar 'c' = C
fromChar 'd' = D
fromChar 'e' = E
fromChar 'f' = F
fromChar 'g' = G
fromChar _ = error "Invalid char"

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

line :: GenParser Char () ([Set Segment], [Set Segment])
line =
  let segment = S.fromList <$> (fromChar <$$> many1 letter)
      segments = many1 (segment <* optional (char ' '))
   in toTuple2 <$> (segments `sepBy1` string "| ")

validPermutations :: [Set Segment] -> [Map Segment Segment]
validPermutations ss =
  [ p
    | p <- permutationMaps,
      all (\s -> applyPermutationMap p s `BM.memberR` digits) ss
  ]

part1 :: Int
part1 =
  $(input 8)
    & parseLinesWith line
    & fmap (length . filter ((`elem` [2, 3, 6, 7]) . S.size) . snd)
    & sum

part2 :: Int
part2 =
  $(input 8)
    & parseLinesWith line
    & fmap (\(as, bs) -> applyPermutationMap (unlist . validPermutations $ as) <$> bs)
    & ((digits BM.!>) <$$>)
    & fmap listAsInt
    & sum
