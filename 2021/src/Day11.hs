module Day11 (part1, part2) where

import Control.Arrow (app)
import Data.List ((!!))
import Data.Map.Strict qualified as M
import Data.Map.Utils (flippedLookupM)
import Data.Set qualified as S
import Helper.Coord (neighbors)
import Helper.Grid (DigitCell (DigitCell), Grid, GridCell, readGrid)
import Helper.TH (input)
import Helper.Util (adjustMany, insertMany)

step :: (Bounded a, Num a, GridCell a) => Grid a -> (Grid a, Int)
step g = second S.size (go ((+ 1) <$> g) S.empty)
  where
    flashIncrement x = if x == minBound then x else x + 1
    go g flashed =
      case [p | p <- flippedLookupM minBound g, not (p `S.member` flashed)] of
        [] -> (g, flashed)
        toFlash ->
          go
            (adjustMany flashIncrement (neighbors =<< toFlash) g)
            (insertMany toFlash flashed)

flashes :: [Int]
flashes =
  let g = readGrid $(input 11) :: Grid DigitCell
   in snd <$> drop 1 (iterate (step . fst) (g, 0))

part1 :: Int
part1 = sum (take 100 flashes)

part2 :: Int
part2 = fst . (!! 0) . dropWhile ((/= 100) . snd) $ zip [1 ..] flashes
