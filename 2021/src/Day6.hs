module Day6 (part1, part2, progeny) where

import Control.Monad.Memo
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

fish :: [Int]
fish = $(input 6) & T.strip & T.splitOn "," & fmap (fst . fromRight (0, "") . decimal)

progeny :: Int -> Int -> Int
progeny d' f' = startEvalMemo $ go (d', f')
  where
    go (0, _) = return 1
    go (d, 0) = do
      p1 <- memo go (d - 1, 6)
      p2 <- memo go (d - 1, 8)
      return $ p1 + p2
    go (d, f) = memo go (d - 1, f - 1)

part1 :: Int
part1 = sum (progeny 80 <$> fish)

part2 :: Int
part2 = sum (progeny 256 <$> fish)
