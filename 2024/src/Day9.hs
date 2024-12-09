module Day9 (part1, part2) where

import Data.RangeSet.Map as RM
import Control.Applicative ((<|>))
import Prelude hiding ((<|>))

part1 =

  let (freeOcc :: [(Int, Int)]) = $(aocx 9) |- ((:) <$> (((0,) . digitToInt <$> digit) <*> many (twoOf (digitToInt <$> digit))))
      ranges freeOcc = foldl' (\(freeRM, occRM, address) (i, (free, occ)) ->
                                 (RM.insertRange ((address, i), ( address + free - 1, i)) freeRM, RM.insertRange ((address + free, i), (address + free + occ - 1, i)) occRM, address + free + occ)) (RM.empty, RM.empty, 0) (zip [0..] freeOcc)
      (free, occ) = ranges freeOcc
      (rocc, rfree) = ranges $ reverse freeOcc
      -- n = sum $ (+) <$@> freeOcc
      n = 100
      -- If occupied then just return
      defragged di  = let 
                            case ((RM.lookupGT (di, di, (-1)) occ) of
                              Just (_, _, i) -> i
                              Nothing -> case (RM.lookupGT (di, di, (-1)) free)) of
                                Just (_, _, i) -> i
                                Nothing -> -1
   in traverse sum [defragged i | i <- [0 .. n - 1]]

part2 :: Text
part2 = "Part 2"
