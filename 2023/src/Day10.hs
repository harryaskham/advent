module Day10 (part1, part2) where

import Data.Bimap qualified as BM
import Data.Map.Strict qualified as M
import Data.Sequence qualified as SQ
import Data.Set qualified as S
import Helper.Coord (Coord2, neighborsNoDiags)
import Helper.Grid (Grid, GridCell (charMap), findOne, readGrid)
import Helper.TH (input)
import Helper.Util (both)
import Relude.Unsafe qualified as U

data Cell = None | Start | PipeVertical | PipeHorizontal | PipeF | Pipe7 | PipeL | PipeJ deriving (Eq, Ord, Show)

instance GridCell Cell where
  charMap = BM.fromList [(None, '.'), (Start, 'S'), (PipeVertical, '|'), (PipeHorizontal, '-'), (PipeF, 'F'), (Pipe7, '7'), (PipeL, 'L'), (PipeJ, 'J')]

pathOutside :: Grid Cell -> (([Coord2], Set Coord2), ([Coord2], Set Coord2))
pathOutside g =
  let pipesTo n@(x, y) c =
        c `elem` case M.lookup n g of
          Just PipeVertical -> [(x, y + 1), (x, y - 1)]
          Just PipeHorizontal -> [(x + 1, y), (x - 1, y)]
          Just PipeF -> [(x + 1, y), (x, y + 1)]
          Just Pipe7 -> [(x - 1, y), (x, y + 1)]
          Just PipeL -> [(x + 1, y), (x, y - 1)]
          Just PipeJ -> [(x - 1, y), (x, y - 1)]
          _ -> []
      outsideOf (x, y) (lx, ly) =
        case ((x - lx, y - ly), M.lookup (x, y) g) of
          ((0, 1), Just PipeVertical) -> [(x + 1, y)]
          ((0, -1), Just PipeVertical) -> [(x - 1, y)]
          ((1, 0), Just PipeHorizontal) -> [(x, y - 1)]
          ((-1, 0), Just PipeHorizontal) -> [(x, y + 1)]
          ((0, -1), Just PipeF) -> [(x - 1, y), (x, y - 1)]
          ((-1, 0), Just PipeF) -> [(x + 1, y + 1)]
          ((1, 0), Just Pipe7) -> [(x + 1, y), (x, y - 1)]
          ((0, -1), Just Pipe7) -> [(x - 1, y + 1)]
          ((-1, 0), Just PipeL) -> [(x, y + 1), (x - 1, y)]
          ((0, 1), Just PipeL) -> [(x + 1, y - 1)]
          ((1, 0), Just PipeJ) -> [(x - 1, y - 1)]
          ((0, 1), Just PipeJ) -> [(x + 1, y), (x, y + 1)]
          _ -> []
      start = findOne Start g
      go path@(c : _) outside l reverse =
        let go' n = go (n : path) (foldl' (flip S.insert) outside (outsideOf n c)) (Just c) reverse
         in case [n | n <- neighborsNoDiags c, Just n /= l, n `pipesTo` c, c == start || c `pipesTo` n] of
              [] -> (path, outside)
              (n : m : _) -> if reverse then go' m else go' n
              (n : _) -> go' n
   in both (go [start] S.empty Nothing) (False, True)

enclosed :: Grid Cell -> Set Coord2 -> Set Coord2 -> Set Coord2
enclosed g path outside =
  let go seen SQ.Empty = (seen, seen)
      go seen (c SQ.:<| cs)
        | c `S.member` (seen <> path) = go seen cs
        | c `S.member` outside = (seen, S.empty)
        | otherwise =
            let ns = [n | n <- neighborsNoDiags c, n `S.notMember` path, n `M.member` g]
             in go (S.insert c seen) (cs SQ.>< SQ.fromList ns)
      goAll _ es [] = es
      goAll seen es (start : starts)
        | start `S.member` seen = goAll seen es starts
        | otherwise =
            let (seen', es') = go S.empty (SQ.singleton start)
             in goAll (seen `S.union` seen') (es `S.union` es') starts
   in goAll S.empty S.empty [c | c <- M.keys g, c `S.notMember` path]

part1 :: Int
part1 =
  $(input 10)
    & readGrid
    & (pathOutside >>> fst >>> length >>> (+ 1) >>> (`div` 2))

part2 :: Int
part2 =
  $(input 10)
    & readGrid
    & ((enclosed >>> uncurry) &&& (pathOutside >>> both (first S.fromList)))
    & uncurry both
    & both (\cs -> if (0, 0) `S.member` cs then S.empty else cs)
    & uncurry S.union
    & S.size