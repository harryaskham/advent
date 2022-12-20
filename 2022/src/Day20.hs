module Day20 (part1, part2) where

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
import Data.Tuple.Extra (fst3, snd3, thd3)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Helper.Coord
import Helper.Grid
import Helper.TH
import Helper.Tracers
import Helper.Util
import Text.ParserCombinators.Parsec

type Ring = Vector (Int, Int, Int)

mkRing :: [Int] -> Ring
mkRing xs = V.fromList $ (\(i, x) -> (x, (i - 1) `mod` length xs, (i + 1) `mod` length xs)) <$> zip [0 ..] xs

mix :: Ring -> Ring
mix r = foldl' mixOnce r [0 .. length r - 1]

mixOnce :: Ring -> Int -> Ring
mixOnce r i
  | n == 0 = r
  | otherwise = traceShow i $ r''
  where
    (n, prevI, nextI) = r V.! i
    (prevN, prevPrevI, _) = r V.! prevI
    (nextN, _, nextNextI) = r V.! nextI
    r' = r V.// [(prevI, (prevN, prevPrevI, nextI)), (nextI, (nextN, prevI, nextNextI))]
    followFwd 0 prevPtr nextPtr = (prevPtr, nextPtr)
    followFwd j _ nextPtr = let (_, _, nextPtr') = r' V.! nextPtr in followFwd (j - 1) nextPtr nextPtr'
    followBwd 0 prevPtr nextPtr = (prevPtr, nextPtr)
    followBwd j prevPtr _ = let (_, prevPtr', _) = r' V.! prevPtr in followBwd (j - 1) prevPtr' prevPtr
    follow = if n < 0 then followBwd else followFwd
    (prevI', nextI') = follow (abs n `mod` (length r - 1)) prevI nextI
    (prevN', prevPrevI', _) = r' V.! prevI'
    (nextN', _, nextNextI') = r' V.! nextI'
    r'' = r' V.// [(i, (n, prevI', nextI')), (prevI', (prevN', prevPrevI', i)), (nextI', (nextN', i, nextNextI'))]

ringToList :: Ring -> [Int]
ringToList r = go 0 zeroI
  where
    (Just zeroI) = V.findIndex ((== 0) . fst3) r
    go j i
      | j == length r = []
      | otherwise =
        let (n, _, nextI) = r V.! i
         in n : go (j + 1) nextI

part1 :: Int
part1 =
  $(input 20)
    & readAs (signed decimal)
    & mkRing
    & mix
    & ringToList
    & cycle
    & (\xs -> (xs !!) <$> [1000, 2000, 3000])
    & sum

part2 :: Int
part2 =
  $(input 20)
    & readAs (signed decimal)
    & fmap (* 811589153)
    & mkRing
    & iterate mix
    & (!! 10)
    & ringToList
    & cycle
    & (\xs -> (xs !!) <$> [1000, 2000, 3000])
    & sum
