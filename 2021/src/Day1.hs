module Day1 (part1, part2) where

import Data.Array qualified as A
import Data.Bimap (Bimap)
import Data.Bimap qualified as BM
import Data.Map.Strict qualified as M
import Data.PQueue.Prio.Min qualified as PQ
import Data.Sequence qualified as SQ
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Read
import Data.Vector qualified as V
import Helper.Coord
import Helper.Grid
import Helper.Tracers
import Helper.Util
import Text.ParserCombinators.Parsec

-- parser :: GenParser Char () [Int]
-- parser = many1 (number <* eol) <* eof

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

part1 :: IO Int
part1 = do
  xs <- readInput decimal (input 1)
  return . length . filter ((>) <$> fst <*> snd) . zip (drop 1 xs) $ xs

part2 :: IO Int
part2 = do
  xs <- readInput decimal (input 1)
  let toList3 (a, b, c) = [a, b, c]
  let ys = sum . toList3 <$> zip3 (drop 2 xs) (drop 1 xs) xs
  return . length . filter ((>) <$> fst <*> snd) . zip (drop 1 ys) $ ys
