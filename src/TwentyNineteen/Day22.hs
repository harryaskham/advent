{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}

module TwentyNineteen.Day22 where

import Control.Monad
import Control.Monad.Memo
import Coord
import qualified Data.Array.Unboxed as A
import Data.Bits
import Data.Char
import qualified Data.Foldable as F
import Data.Function
import Data.List
import Data.List.Extra
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Mod
import Data.Monoid
import Data.Ord
import Data.Ratio
import Data.Sequence (Seq)
import qualified Data.Sequence as SQ
import Data.Set (Set)
import qualified Data.Set as S
import Data.Tuple.Extra
import Data.Vector (Vector)
import qualified Data.Vector as V
import Debug.Trace
import GHC.Natural
import Grid
import Text.ParserCombinators.Parsec
import Util

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

undoDealInto :: Integer -> Integer -> Integer
undoDealInto l x = l - x - 1

undoDealWith :: Integer -> Integer -> Integer -> Integer
undoDealWith l inc x = numerator . head . filter ((== 1) . denominator) $ kLen
  where
    kLen = [(x % inc) + (k * l % inc) | k <- [0 ..]]

undoCut :: Integer -> Integer -> Integer -> Integer
undoCut l n x
  | n > 0 && x > (l - n - 1) = (x - (l - n))
  | n > 0 = (x + n)
  | n < 0 && x < (negate n) = (l + n + x)
  | n < 0 = (x + n)

undoShuffles :: Integer -> GenParser Char () (Integer -> Integer)
undoShuffles l = foldl1 (.) <$> many op <* eof
  where
    op = ((try dealIntoOp) <|> (try dealWithOp) <|> (try cutOp)) <* eol
    dealIntoOp = string "deal into new stack" >> return (undoDealInto l)
    dealWithOp = undoDealWith l <$> (string "deal with increment " *> number)
    cutOp = undoCut l <$> (string "cut " *> number)

bwdDealInto :: MInt -> MInt
bwdDealInto x = negate x - 1

bwdDealWith :: MInt -> MInt -> MInt
bwdDealWith n x = x / n

bwdCut :: MInt -> MInt -> MInt
bwdCut n x = x + n

bwdShuffles :: GenParser Char () (MInt -> MInt)
bwdShuffles = foldl1 (.) <$> many op <* eof
  where
    op = ((try dealIntoOp) <|> (try dealWithOp) <|> (try cutOp)) <* eol
    dealIntoOp = string "deal into new stack" >> return bwdDealInto
    dealWithOp = bwdDealWith <$> (string "deal with increment " *> (fromIntegral <$> number))
    cutOp = bwdCut <$> (string "cut " *> (fromIntegral <$> number))

fwdDealInto :: MInt -> MInt
fwdDealInto x = negate x - 1

fwdDealWith :: MInt -> MInt -> MInt
fwdDealWith = (*)

fwdCut :: MInt -> MInt -> MInt
fwdCut n x = x - n

fwdShuffles :: GenParser Char () (MInt -> MInt)
fwdShuffles = foldl1 (.) . reverse <$> many op <* eof
  where
    op = ((try dealIntoOp) <|> (try dealWithOp) <|> (try cutOp)) <* eol
    dealIntoOp = string "deal into new stack" >> return fwdDealInto
    dealWithOp = fwdDealWith <$> (string "deal with increment " *> (fromIntegral <$> number))
    cutOp = fwdCut <$> (string "cut " *> (fromIntegral <$> number))

getCycle :: (Eq a, Show a) => (a -> a) -> a -> [a]
getCycle f start = go start []
  where
    go !x !cyc
      | not (null cyc) && x == start = reverse cyc
      | otherwise = traceShow (length cyc, x) $ go (f x) (x : cyc)

part2 :: IO ()
part2 = forward

backward :: IO ()
backward = do
  let l = 119315717514047
      n = 101741582076661
  undoShuffle <- readWithParser (undoShuffles l) <$> input 2019 22
  let cyc = getCycle undoShuffle 2020
  print $ length cyc
  print (cyc !! (n `mod` (length cyc)))

type MInt = Mod 119315717514047

--type MInt = Mod 10007

backward' :: IO ()
backward' = do
  let n = 101741582076661
      --let n = 10006
      target = 2020
  bwdShuffle <- readWithParser bwdShuffles <$> input 2019 22
  let cyc =
        takeWhile
          ((/= target) . snd . traceShowIdWhen ((== 0) . (`mod` 100000) . (fst)))
          (drop 1 $ zip [0 ..] (iterate' bwdShuffle target))
      cyc' = 2020 : (snd <$> cyc)
  print (cyc' !! (n `mod` (length cyc')))

forward :: IO ()
forward = do
  let n = 101741582076661
      target = 2003
  fwdShuffle <- readWithParser fwdShuffles <$> input 2019 22
  -- let cyc =
  --takeWhile ((/= target) . snd . traceShowIdWhen ((== 0) . (`mod` 100000) . (fst))) (drop 1 $ zip [0 ..] (iterate' fwdShuffle target))
  --let cyc' = target : (snd <$> cyc)
  --print $ cyc' !! ((length cyc' - n) `mod` length cyc')
  let cycLen =
        fst . head . filter ((== target) . snd . traceShowIdWhen ((== 0) . (`mod` 100000) . (fst))) $
          (drop 1 $ zip [0 ..] (iterate' fwdShuffle target))
  print $ cycLen
  print $ iterate' fwdShuffle target !! ((cycLen - n) `mod` cycLen)

-- let cyc = getCycle fwdShuffle 2020
