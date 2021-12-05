module Day5 (part1, part2) where

import Data.Map.Strict qualified as M
import Helper.Coord (Coord2, linePoints)
import Helper.TH (input)
import Helper.Util (both, coord2, countMap, parseLinesWith, same, split, toTuple2)
import Text.ParserCombinators.Parsec (GenParser, sepBy, string)

solve :: ((Coord2, Coord2) -> Bool) -> Int
solve f =
  $(input 5)
    & parseLinesWith (toTuple2 . sort <$> coord2 `sepBy` string " -> ")
    & filter f
    & concatMap linePoints
    & countMap
    & M.filter (> 1)
    & M.size

part1 :: Int
part1 = solve (split fst &&& split snd >>> both same >>> uncurry (||))

part2 :: Int
part2 = solve (const True)
