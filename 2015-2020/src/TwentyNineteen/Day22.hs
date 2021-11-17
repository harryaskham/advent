{-# LANGUAGE DataKinds #-}

module TwentyNineteen.Day22 where

import qualified Data.Array.Unboxed as A
import qualified Data.Foldable as F
import Data.Mod (Mod (..), (^%))
import Data.Sequence (Seq)
import qualified Data.Sequence as SQ
import Text.ParserCombinators.Parsec
  ( GenParser,
    eof,
    many,
    string,
    try,
    (<|>),
  )
import Util (eol, input, number, readWithParser, unjust)

cut :: Int -> Seq Int -> Seq Int
cut n xs
  | n < 0 = let (a, b) = SQ.splitAt (length xs - negate n) xs in b SQ.>< a
  | otherwise = let (a, b) = SQ.splitAt n xs in b SQ.>< a

dealWith :: Int -> Seq Int -> Seq Int
dealWith inc xs = SQ.fromList . A.elems $ newDeck
  where
    newDeck :: A.Array Int Int
    newDeck =
      A.array
        (0, length xs - 1)
        [ ((i * inc) `mod` length xs, x)
          | (i, x) <- zip [0 ..] (F.toList xs)
        ]

shuffles :: GenParser Char () (Seq Int -> Seq Int)
shuffles = foldl1 (.) . reverse <$> many op <* eof
  where
    op = ((try dealIntoOp) <|> (try dealWithOp) <|> (try cutOp)) <* eol
    dealIntoOp = string "deal into new stack" >> return SQ.reverse
    dealWithOp = dealWith <$> (string "deal with increment " *> number)
    cutOp = cut <$> (string "cut " *> number)

part1 :: IO Int
part1 = do
  shuffle <- readWithParser shuffles <$> input 2019 22
  return . unjust $ SQ.elemIndexL 2019 (shuffle $ SQ.fromList [0 .. 10006])

type MInt = Mod 119315717514047

bwdDealInto :: MInt -> MInt
bwdDealInto = subtract 1 . negate

bwdDealWith :: MInt -> MInt -> MInt
bwdDealWith = flip (/)

bwdCut :: MInt -> MInt -> MInt
bwdCut = (+)

bwdShuffles :: GenParser Char () (MInt -> MInt)
bwdShuffles = foldl1 (.) <$> many op <* eof
  where
    op = ((try dealIntoOp) <|> (try dealWithOp) <|> (try cutOp)) <* eol
    dealIntoOp = string "deal into new stack" >> return bwdDealInto
    dealWithOp = bwdDealWith <$> (string "deal with increment " *> (fromIntegral <$> number))
    cutOp = bwdCut <$> (string "cut " *> (fromIntegral <$> number))

part2 :: IO MInt
part2 = do
  bwdShuffle <- readWithParser bwdShuffles <$> input 2019 22
  let r = bwdShuffle 1 - bwdShuffle 0
      a = bwdShuffle 0
      bwdShuffleN n x = a * (1 - r ^% n) / (1 - r) + x * r ^% n
  return $ bwdShuffleN 101741582076661 2020
