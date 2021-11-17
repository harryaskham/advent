module TwentySixteen.Day1 where

import Coord (Dir2 (DirUp), manhattan0, move, rlToTurn)
import Data.List (foldl')
import Data.List.Split (splitOn)
import qualified Data.Set as S
import Data.Tuple.Extra (fst3, thd3)

inputPath :: String
inputPath = "input/2016/1.txt"

input :: IO [(Dir2 -> Dir2, Int)]
input =
  concatMap
    ( \(rl : n) ->
        (rlToTurn rl, 1) : replicate (read n - 1) (id, 1)
    )
    . splitOn ", "
    . head
    . lines
    <$> readFile inputPath

part1 :: IO Int
part1 =
  manhattan0
    . fst
    . foldl'
      ( \(pos, dir) (turn, n) ->
          (move (turn dir) n pos, turn dir)
      )
      ((0, 0), DirUp)
    <$> input

part2 :: IO Int
part2 =
  manhattan0
    . fst3
    . head
    . filter (S.member <$> fst3 <*> thd3)
    . scanl
      ( \(pos, dir, seen) (turn, n) ->
          (move (turn dir) n pos, turn dir, S.insert pos seen)
      )
      ((0, 0), DirUp, S.empty)
    <$> input
