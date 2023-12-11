module Day11 (part1, part2) where

import Data.Bimap qualified as BM
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Helper.Coord (Coord2)
import Helper.Grid (Grid, GridCell (charMap), find, maxXY, readGrid)
import Helper.TH (input)
import Relude.Unsafe qualified as U
import Prelude hiding (find)

data Cell = None | Star deriving (Eq, Ord)

instance GridCell Cell where
  charMap = BM.fromList [(None, '.'), (Star, '#')]

pairwiseDistances :: Int -> Grid Cell -> Map Coord2 (Map Coord2 Int)
pairwiseDistances scale g =
  let stars = find Star g
      (maxX, maxY) = maxXY g
      emptyX = S.fromList [x | x <- [0 .. maxX], all (== None) [g M.! (x, y) | y <- [0 .. maxY]]]
      emptyY = S.fromList [y | y <- [0 .. maxY], all (== None) [g M.! (x, y) | x <- [0 .. maxX]]]
      distance (ax, ay) (bx, by) =
        let xd = sum [delta | x <- [min ax bx .. max ax bx], let delta = if x `S.member` emptyX then scale else 1]
            yd = sum [delta | y <- [min ay by .. max ay by], let delta = if y `S.member` emptyY then scale else 1]
         in xd + yd - 2
   in M.fromList
        <$> M.fromListWith
          (<>)
          ( mconcat
              [ [(a, [(b, d)]), (b, [(a, d)])]
                | (i, a) <- zip [1 ..] stars,
                  b <- drop i stars,
                  let d = distance a b
              ]
          )

solve :: Int -> Int
solve scale =
  $(input 11)
    & readGrid
    & pairwiseDistances scale
    & (\d -> [d M.! a M.! b | let stars = M.keys d, (i, a) <- zip [1 ..] stars, b <- drop i stars])
    & sum

part1 :: Int
part1 = solve 2

part2 :: Int
part2 = solve 1000000