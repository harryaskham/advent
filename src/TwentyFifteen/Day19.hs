module TwentyFifteen.Day19 where

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

type Replacement = (String, String)

replacements :: GenParser Char () (Map String [String], String)
replacements = do
  rs <- many (replacement <* eol)
  eol
  target <- many1 letter <* eol
  eof
  return (M.fromListWith (++) rs, target)
  where
    replacement = do
      a <- many1 letter
      string " => "
      b <- many1 letter
      return (a, [b])

singleReplacements :: Map String [String] -> String -> [String]
singleReplacements rs xs =
  nub . concat $
    [ concat [xsA, xsB, xsAB]
      | (a, b) <- case xs of
          [x] -> [(x, x)]
          _ -> zip xs (drop 1 xs),
        let xsA =
              case M.lookup [a] rs of
                Just a' -> concat $ replaceOnes [a] <$> a' <*> pure xs
                Nothing -> []
            xsB =
              case M.lookup [b] rs of
                Just b' -> concat $ replaceOnes [b] <$> b' <*> pure xs
                Nothing -> []
            xsAB =
              case M.lookup [a, b] rs of
                Just ab' -> concat $ replaceOnes [a, b] <$> ab' <*> pure xs
                Nothing -> []
    ]

part1 :: IO Int
part1 = do
  (rs, target) <- readWithParser replacements <$> input 2015 19
  return . length $ singleReplacements rs target

-- TODO: Correct but slow, needs a better guided expansion
shortestConstruction :: Map String [String] -> String -> Maybe Int
shortestConstruction rs target = go (SQ.singleton ("e", 0)) S.empty
  where
    go SQ.Empty _ = Nothing
    go ((current, steps) SQ.:<| queue) seen
      | current == target = Just steps
      | current `S.member` seen = go queue seen
      | otherwise = traceShow steps $ go (queue SQ.>< nextStates) (S.insert current seen)
      where
        nextStates = SQ.fromList [(s, steps + 1) | s <- singleReplacements rs current]

shortestConstructionRev :: Map String [String] -> String -> Maybe Int
shortestConstructionRev rs' target = go (SQ.singleton (target, 0)) S.empty
  where
    rs = M.fromListWith (++) [(b, [a]) | (a, bs) <- M.toList rs', b <- bs]
    go SQ.Empty _ = Nothing
    go ((current, steps) SQ.:<| queue) seen
      | current == "e" = Just steps
      | current `S.member` seen = go queue seen
      | otherwise = traceShow steps $ go (queue SQ.>< nextStates) (S.insert current seen)
      where
        nextStates = SQ.fromList [(s, steps + 1) | s <- singleReplacements rs current]

part2 :: IO (Maybe Int)
part2 = do
  (rs, target) <- readWithParser replacements <$> input 2015 19
  return $ shortestConstructionRev rs target
