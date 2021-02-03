{-# LANGUAGE BangPatterns #-}

module TwentySixteen.Day19 where

import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Sequence (Seq)
import qualified Data.Sequence as SQ
import Data.Tuple.Extra (swap)

start :: Int
start = 3001330

mkToLeft :: Map Int Int
mkToLeft = M.fromList (zip [1 .. start] (drop 1 $ cycle [1 .. start]))

step :: Map Int Int -> Int -> Int
step !toLeft !elf
  | M.size toLeft == 1 = elf
  | otherwise = step (M.delete stealFrom . M.insert elf nextElf $ toLeft) nextElf
  where
    stealFrom = toLeft M.! elf
    nextElf = toLeft M.! stealFrom

part1 :: Int
part1 = step mkToLeft 1

stepCircular :: Map Int Int -> Map Int Int -> Seq Int -> Int -> Int -> Int
stepCircular !toLeft !toRight !elves !elf !elfIx
  | M.size toLeft == 1 = elf
  | otherwise =
    stepCircular nextToLeft nextToRight nextElves nextElf nextElfIx
  where
    stealFromIx = (elfIx + (length elves `div` 2)) `mod` length elves
    stealFrom = elves `SQ.index` stealFromIx
    nextElves = SQ.deleteAt stealFromIx elves
    rightOfStolen = toRight M.! stealFrom
    leftOfStolen = toLeft M.! stealFrom
    nextToLeft = M.delete stealFrom . M.insert rightOfStolen leftOfStolen $ toLeft
    nextToRight = M.delete stealFrom . M.insert leftOfStolen rightOfStolen $ toRight
    nextElf = nextToLeft M.! elf
    nextElfIx =
      if stealFromIx < elfIx
        then elfIx `mod` length nextElves
        else (elfIx + 1) `mod` length nextElves

mkToRight :: Map Int Int
mkToRight = M.fromList . fmap swap . M.toList $ mkToLeft

part2 :: Int
part2 = stepCircular mkToLeft mkToRight (SQ.fromList [1 .. start]) 1 0
