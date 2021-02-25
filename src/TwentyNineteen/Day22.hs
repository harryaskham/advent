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
undoDealWith l inc x = numerator . head $ filter ((== 1) . denominator) kLen
  where
    kLen = [(x % inc) + (k * l % inc) | k <- [0 ..]]

undoCut :: Integer -> Integer -> Integer -> Integer
undoCut l n x
  | n > 0 && x > (l - n) = x - (l - n)
  | n > 0 = x + n
  | n < 0 && x < (negate n) = l + n + x
  | n < 0 = x + n

followPosition :: Integer -> GenParser Char () (Integer -> Integer)
followPosition l = foldl1 (.) <$> many op <* eof
  where
    op = ((try dealIntoOp) <|> (try dealWithOp) <|> (try cutOp)) <* eol
    dealIntoOp = string "deal into new stack" >> return (undoDealInto l)
    dealWithOp = undoDealWith l <$> (string "deal with increment " *> number)
    cutOp = undoCut l <$> (string "cut " *> number)

-- TODO: if this finds a cycle insyead we need a map from pos to last place we found it
getCycle :: (Integer -> Integer) -> Integer -> [Integer]
getCycle f pos = go pos S.empty []
  where
    go x seen cyc
      | x `S.member` seen = reverse cyc
      | otherwise = traceShow (S.size seen) go (f x) (S.insert x seen) (x : cyc)

part2 :: IO Integer
part2 = do
  f <- readWithParser (followPosition 119315717514047) <$> input 2019 22
  let cyc = getCycle f 2020
  return (cyc !! (101741582076661 `mod` (length cyc)))

test :: IO ()
test = do
  f <- readWithParser (followPosition 10007) <$> input 2019 22
  print $ f 8775
