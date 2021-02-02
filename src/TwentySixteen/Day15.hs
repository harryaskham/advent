module TwentySixteen.Day15 where

import Control.Monad
import Control.Monad.Memo
import Coord
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

data Disc = Disc Int Int Int deriving (Show)

discs :: GenParser Char () [Disc]
discs = do
  ds <- many1 disc
  eof
  return ds
  where
    disc = do
      string "Disc #"
      dId <- digitToInt <$> digit
      string " has "
      positions <- read <$> many1 digit
      string " positions; at time=0, it is at position "
      position <- digitToInt <$> digit
      string "."
      eol
      return $ Disc dId positions position

configuration :: [Disc] -> Int -> [Int]
configuration ds t =
  [ (position + dId + t) `mod` positions
    | (Disc dId positions position) <- ds
  ]

solve :: [Disc] -> Int
solve ds = head $ filter ((== [0]) . nub . configuration ds) [0 ..]

part1 :: IO Int
part1 = do
  ds <- readWithParser discs <$> input 2016 15
  return . solve $ ds

part2 :: IO Int
part2 = do
  ds <- readWithParser discs <$> input 2016 15
  let ds' = ds ++ [Disc (length ds + 1) 11 0]
  return . solve $ ds'
