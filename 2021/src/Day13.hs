module Day13 (part1, part2) where

import Data.Array qualified as A
import Data.Bimap (Bimap)
import Data.Bimap qualified as BM
import Data.List ((!!))
import Data.Map.Strict qualified as M
import Data.Mod
import Data.PQueue.Prio.Min qualified as PQ
import Data.Sequence qualified as SQ
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Read
import Data.Vector qualified as V
import Helper.Coord
import Helper.Grid
import Helper.TH
import Helper.Tracers
import Helper.Util
import Text.ParserCombinators.Parsec
import Prelude hiding ((<|>))

data Fold = FoldX Int | FoldY Int deriving (Eq, Ord, Show)

parser :: GenParser Char () (Set (Int, Int), [Fold])
parser = do
  dots <- many1 (number `sepBy1` char ',' <* eol) <* eol
  folds <- many1 do
    string "fold along "
    f <- (string "x=" *> (FoldX <$> number)) <|> (string "y=" *> (FoldY <$> number))
    eol
    return f
  eof
  return (S.fromList (toTuple2 <$> dots), folds)

-- line :: GenParser Char () Int
-- line = number

-- data Cell
--   = Empty
--   | Wall
--   deriving (Eq, Ord)

-- instance GridCell Cell where
--   charMap =
--     BM.fromList
--       [ (Empty, ' '),
--         (Wall, '#')
--       ]

doFold :: Set (Int, Int) -> Fold -> Set (Int, Int)
doFold ps (FoldX c) = S.map (\(x, y) -> if x > c then (c * 2 - x, y) else (x, y)) ps
doFold ps (FoldY c) = S.map (\(x, y) -> if y > c then (x, c * 2 - y) else (x, y)) ps

part1 :: Int
part1 = $(input 13) & parseWith parser & second (!! 0) & uncurry doFold & S.size

part2 :: Text
part2 = $(input 13) & parseWith parser & uncurry (foldl' doFold) & fromCoords Wall & pretty
