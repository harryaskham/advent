module TwentyEighteen.Day12 where

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

parseInput :: GenParser Char () (Set Int, Map String Char)
parseInput = do
  string "initial state: "
  initial <- many1 (oneOf "#.")
  eol >> eol
  rules <- many1 rule
  eof
  return (S.fromList $ elemIndices '#' initial, M.fromList rules)
  where
    rule = do
      rs <- count 5 (oneOf "#.")
      string " => "
      c <- oneOf "#."
      eol
      return (rs, c)

segmentAtCenter :: Set Int -> Int -> String
segmentAtCenter line c = [if x `S.member` line then '#' else '.' | x <- [c - 2 .. c + 2]]

stepLine :: Map String Char -> Set Int -> Set Int
stepLine rules line =
  S.fromList
    [ c
      | c <- [S.findMin line - 2 .. S.findMax line + 2],
        rules M.! segmentAtCenter line c == '#'
    ]

run :: Map String Char -> Int -> Set Int -> Set Int
run _ 0 line = line
run rules n line = run rules (n - 1) (stepLine rules line)

part1 :: IO Int
part1 = do
  (initial, rules) <- readWithParser parseInput <$> input 2018 12
  return . sum $ run rules 20 initial

part2 :: IO Int
part2 = do
  (initial, rules) <- readWithParser parseInput <$> input 2018 12
  let stable = run rules 145 initial
  return $ sum (S.map ((+ 50000000000) . subtract 145) stable)
