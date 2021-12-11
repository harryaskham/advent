module Day11 (part1, part2) where

import Data.List (nub, (!!))
import Data.Map.Strict qualified as M
import Data.Map.Utils (flippedLookupM)
import Data.Set qualified as S
import Data.Tuple.Extra (fst3, thd3, uncurry3)
import Helper.Coord (neighbors)
import Helper.Grid (DigitCell, Grid, GridCell, readGrid)
import Helper.TH (input)

step :: (Bounded a, Num a, GridCell a) => (Grid a, Int) -> (Grid a, Int)
step (g, flashes) = second ((+ flashes) . S.size) $ go ((+ 1) <$> g) S.empty
  where
    flashIncrement x
      | x == minBound = x
      | otherwise = x + 1
    go g flashed
      | null toFlash = (g, flashed)
      | otherwise = uncurry go $ foldl' flash (g, flashed) toFlash
      where
        toFlash = [p | p <- flippedLookupM minBound g, not (p `S.member` flashed)]
        flash (g, flashed) p
          | p `S.member` flashed = (g, flashed)
          | otherwise = (foldl' (flip (M.adjust flashIncrement)) g (neighbors p), S.insert p flashed)

part1 :: Int
part1 = snd . (!! 100) $ iterate step (readGrid $(input 11) :: Grid DigitCell, 0)

part2 :: Int
part2 =
  let fs = snd <$> iterate step (readGrid $(input 11) :: Grid DigitCell, 0)
   in fst . (!! 0) . dropWhile ((/= 100) . snd) $
        zip [1 ..] (uncurry (-) <$> zip (drop 1 fs) fs)
