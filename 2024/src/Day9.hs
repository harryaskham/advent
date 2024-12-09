module Day9 (part1, part2) where

import Data.RangeSet.Map as RM
import Control.Applicative ((<|>))
import Prelude hiding ((<|>))

part1 =
  let freeOcc = $(aoc 9) |- ((:) <$> (((0,) . digitToInt <$> digit) <*> many (twoOf (digitToInt <$> digit))))
      (free, occ) = foldl' (\(freeRM, occRM, address) (i, (free, occ)) -> (RM.insertRange ((address, i), ( address + free - 1, i)) freeRM, RM.insertRange ((address + free, i), (address + free + occ - 1, i)) occRM, address + free + occ)) (RM.empty, RM.empty, 0) (zip [0..] freeOcc)
      n = sum (uncurry (+) <$> freeOcc)
      -- If occupied then just return 
      defragged di  = let Just (_, _, i) = ((RM.lookupGT (di, di, (-1)) occ) Control.Applicative.<|> (RM.lookupGT (di, di, (-1)) free)) in i
   in sum [defragged i | i <- [0 .. n - 1]]

part2 :: Text
part2 = "Part 2"
