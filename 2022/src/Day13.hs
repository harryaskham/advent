module Day13 (part1, part2) where

import Data.List.Extra (findIndices)
import Helper.TH (input)
import Helper.Util (eol, number, parseWith, toTuple2)
import Text.ParserCombinators.Parsec (Parser, between, char, count, eof, sepBy, (<|>))
import Prelude hiding ((<|>))

data Packet = PInt Int | PList [Packet] deriving (Eq, Show)

parser :: Parser [(Packet, Packet)]
parser = (packetPair `sepBy` eol) <* eof
  where
    packetPair = toTuple2 <$> count 2 (packet <* eol)
    packet = (PInt <$> number) <|> (PList <$> between (char '[') (char ']') (packet `sepBy` char ','))

cmp :: Packet -> Packet -> Ordering
cmp (PInt a) (PInt b) = compare a b
cmp (PList a) (PInt b) = cmp (PList a) (PList [PInt b])
cmp (PInt a) (PList b) = cmp (PList [PInt a]) (PList b)
cmp (PList []) (PList []) = EQ
cmp (PList []) (PList _) = LT
cmp (PList _) (PList []) = GT
cmp (PList (a : as)) (PList (b : bs)) =
  case cmp a b of
    EQ -> cmp (PList as) (PList bs)
    LT -> LT
    GT -> GT

part1 :: Int
part1 =
  $(input 13)
    & parseWith parser
    & zip [1 ..]
    & filter ((== LT) . uncurry cmp . snd)
    & fmap fst
    & sum

part2 :: Int
part2 =
  let dividers = [PList [PList [PInt 2]], PList [PList [PInt 6]]]
   in $(input 13)
        & parseWith parser
        & concatMap (\(a, b) -> [a, b])
        & (++ dividers)
        & sortBy cmp
        & findIndices (`elem` dividers)
        & fmap (+ 1)
        & product
