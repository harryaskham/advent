module Day23 (part1, part2) where

import Data.Bimap (Bimap)
import Data.Bimap qualified as BM
import Data.List ((!!))
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Helper.Coord (Coord2, Dir2 (..))
import Helper.Grid (Grid, GridCell (charMap), alignTo0, fillEmpty, find, readGrid)
import Helper.TH (input)
import Helper.Util (countMap)
import Relude.Unsafe qualified as U
import Prelude hiding (find)

data Cell = None | Elf deriving (Eq, Ord, Bounded)

instance GridCell Cell where
  charMap = BM.fromList [(None, '.'), (Elf, '#')]

run :: [Dir2] -> Set Coord2 -> [Set Coord2]
run dirs elves = elves : run (drop 1 dirs) elves'
  where
    dirOrder = take 4 dirs
    testDs ds = if any (`S.member` elves) ds then Nothing else Just (ds !! 1)
    proposals (x, y) dir =
      case dir of
        DirUp -> testDs [(x - 1, y - 1), (x, y - 1), (x + 1, y - 1)]
        DirDown -> testDs [(x - 1, y + 1), (x, y + 1), (x + 1, y + 1)]
        DirLeft -> testDs [(x - 1, y - 1), (x - 1, y), (x - 1, y + 1)]
        DirRight -> testDs [(x + 1, y - 1), (x + 1, y), (x + 1, y + 1)]
    proposal elf = case mapMaybe (proposals elf) dirOrder of
      [] -> Nothing
      [_, _, _, _] -> Nothing
      (p : _) -> Just p
    elfProposals = [(elf, proposal elf) | elf <- S.toList elves]
    elfToProposal = M.fromList elfProposals
    proposalCounts = countMap (snd <$> elfProposals)
    moveElf elf =
      case elfToProposal M.! elf of
        Nothing -> elf
        Just p ->
          case proposalCounts M.! Just p of
            1 -> p
            _ -> elf
    elves' = S.map moveElf elves

steps :: [Set Coord2]
steps = run dirs (S.fromList elves)
  where
    g = readGrid $(input 23) :: Grid Cell
    elves = find Elf g
    dirs = cycle [DirUp, DirDown, DirLeft, DirRight]

part1 :: Int
part1 =
  let g = fillEmpty $ alignTo0 $ M.fromList ((,Elf) <$> S.toList (steps !! 10))
   in length $ find None g

part2 :: Int
part2 = U.head [i | (i, (e1, e2)) <- zip [1 ..] (zip steps (drop 1 steps)), e1 == e2]
