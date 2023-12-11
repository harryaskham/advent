module Day11 (part1, part2) where

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
import Relude.Unsafe qualified as U
import Text.ParserCombinators.Parsec
import Prelude hiding (find)

data Cell = None | Star deriving (Eq, Ord)

instance GridCell Cell where
  charMap = BM.fromList [(None, '.'), (Star, '#')]

-- pairwiseDistances' :: Grid Cell -> Map Coord2 (Map Coord2 Int)
-- pairwiseDistances' g =
--   let stars = find Star g
--       (maxX, maxY) = maxXY g
--       emptyX = S.fromList [x | x <- [0 .. maxX], all (== None) [g M.! (x, y) | y <- [0 .. maxY]]]
--       emptyY = S.fromList [y | y <- [0 .. maxY], all (== None) [g M.! (x, y) | x <- [0 .. maxX]]]
--       go seen t ((c@(x, y), (lx, ly), d) SQ.:<| q)
--         | c == t = d
--         | c `S.member` seen || c `M.notMember` g = go seen t q
--         | otherwise = go (S.insert c seen) t (q SQ.>< SQ.fromList ((,c,d + delta) <$> neighborsNoDiags c))
--         where
--           delta = case (abs (x - lx), abs (y - ly), x `S.member` emptyX, y `S.member` emptyY) of
--             (1, 0, True, _) ->
--             (0, 1, _, True) -> 2
--             _ -> 1
--    in M.fromList
--         <$> M.fromListWith
--           (<>)
--           ( mconcat
--               [ [(a, [(b, d)]), (b, [(a, d)])]
--                 | (i, a) <- zip [1 ..] stars,
--                   b <- drop i stars,
--                   let d = go S.empty b (SQ.singleton (a, a, 0))
--               ]
--           )

pairwiseDistances :: Grid Cell -> Map Coord2 (Map Coord2 Int)
pairwiseDistances g =
  let stars = find Star g
      (maxX, maxY) = maxXY g
      emptyX = S.fromList [x | x <- [0 .. maxX], all (== None) [g M.! (x, y) | y <- [0 .. maxY]]]
      emptyY = S.fromList [y | y <- [0 .. maxY], all (== None) [g M.! (x, y) | x <- [0 .. maxX]]]
      distance (ax, ay) (bx, by) =
        let xd = sum [delta | x <- [min ax bx .. max ax bx], let delta = if x `S.member` emptyX then 2 else 1]
            yd = sum [delta | y <- [min ay by .. max ay by], let delta = if y `S.member` emptyY then 2 else 1]
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

shortestPath :: Grid Cell -> Int
shortestPath g =
  let distances = pairwiseDistances g
      stars = M.keys distances
      go q
        | S.size seen == length stars = d
        | c `S.member` seen = go q'
        | otherwise =
            traceShow (d, S.size seen, PQ.size q) $
              go (foldl' (\q (n, nd) -> PQ.insert (d + nd) (n, S.insert c seen) q) q' (M.toList $ distances M.! c))
        where
          ((d, (c, seen)), q') = PQ.deleteFindMin q
   in go (foldl' (\q c -> PQ.insert 0 (c, S.empty) q) PQ.empty stars)

part1 :: Int
part1 =
  $(input 11)
    -- \$(exampleInput 11)
    & readGrid
    & pairwiseDistances
    -- & traceShowId
    & (\d -> [d M.! a M.! b | let stars = M.keys d, (i, a) <- zip [1 ..] stars, b <- drop i stars])
    & sum

part2 :: Text
part2 = "Part 2"
