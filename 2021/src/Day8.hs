module Day8 (part1, part2) where

import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Helper.TH (input)
import Helper.Util (count, listAsInt, parseLinesWith, permutationMaps, permuteSet, toTuple2, (<$$>))
import Text.ParserCombinators.Parsec (GenParser, char, letter, many1, sepBy1, string)

data Segment = A | B | C | D | E | F | G deriving (Eq, Ord, Enum)

fromChar :: Char -> Segment
fromChar = (M.fromList (zip ['a' .. 'g'] [A .. G]) M.!)

digits :: Map (Set Segment) Int
digits =
  M.fromList . flip zip [0 ..] . fmap S.fromList $
    [ [A, B, C, E, F, G],
      [C, F],
      [A, C, D, E, G],
      [A, C, D, F, G],
      [B, C, D, F],
      [A, B, D, F, G],
      [A, B, D, E, F, G],
      [A, C, F],
      [A, B, C, D, E, F, G],
      [A, B, C, D, F, G]
    ]

validPermutations :: [Set Segment] -> [Map Segment Segment]
validPermutations ss = do
  p <- permutationMaps
  guard $ all (`M.member` digits) (permuteSet p <$> ss)
  return p

line :: GenParser Char () [Int]
line = do
  let segment = S.fromList <$> (fromChar <$$> many1 letter)
      segments = many1 (segment <* optional (char ' '))
  (p : _) <- validPermutations <$> (segments <* string "| ")
  (digits M.!) . permuteSet p <$$> segments

part1 :: Int
part1 = parseLinesWith line $(input 8) & fmap (count (`elem` [1, 4, 7, 8])) & sum

part2 :: Int
part2 = parseLinesWith line $(input 8) & fmap listAsInt & sum
