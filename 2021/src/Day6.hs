module Day6 (part1, part2) where

import Data.Array qualified as A
import Data.Bimap (Bimap)
import Data.Bimap qualified as BM
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

part1 :: Text
part1 =
  -- Dynamic Input:
  -- do
  --   xs <- readInputIO (signed decimal) (inputPath 6)
  --   xs <- parseWithIO parser (inputPath 6)
  --   xs <- lines <$> readFileText (inputPath 6)
  --   grid <- readGridIO (inputPath 6) :: (IO (Grid Cell))
  -- Compiled Input:
  -- let xs = readAs (signed decimal) $(input 6)
  -- let xs = parseWith parser $(input 6)
  -- let xs = lines $(input 6)
  -- let grid = readGrid $(input 6) :: (Grid Cell)
  "Part 1"

part2 :: Text
part2 = "Part 2"
