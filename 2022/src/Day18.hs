module Day18 (part1, part2) where

import Data.Map.Strict qualified as M
import Data.Sequence qualified as SQ
import Data.Set qualified as S
import Data.Tuple.Extra (fst3, snd3, thd3)
import Helper.Coord (Coord3, neighbors3NoDiags)
import Helper.TH (input)
import Helper.Util (countMap, numberLine3, parseWith)
import Text.ParserCombinators.Parsec (eof, many1)

surfaceArea :: Set Coord3 -> Int
surfaceArea ps = sum [area p | p <- S.toList ps]
  where
    area (x, y, z) = 6 - length [n | n <- neighbors3NoDiags (x, y, z), n `S.member` ps]

exteriorArea :: Coord3 -> Set Coord3 -> Maybe Int
exteriorArea start ps =
  case interiorArea start ps of
    Just interior ->
      let area (x, y, z) =
            6 - length [n | n <- neighbors3NoDiags (x, y, z), n `S.member` ps]
              - length [n | n <- neighbors3NoDiags (x, y, z), n `S.member` interior]
       in Just $ sum [area p | p <- S.toList ps]
    Nothing -> Nothing

interiorArea :: Coord3 -> Set Coord3 -> Maybe (Set Coord3)
interiorArea start ps = go (SQ.singleton start) S.empty
  where
    go SQ.Empty seen = Just seen
    go (current SQ.:<| queue) seen
      | S.size seen > 3500 = Nothing
      | current `S.member` seen = go queue seen
      | current `S.member` ps = go queue seen
      | otherwise = go queue' seen'
      where
        seen' = S.insert current seen
        queue' = queue SQ.>< SQ.fromList (neighbors3NoDiags current)

part1 :: Int
part1 =
  $(input 18)
    & parseWith (many1 numberLine3 <* eof)
    & S.fromList
    & surfaceArea

part2 :: Int
part2 =
  $(input 18)
    & parseWith (many1 numberLine3 <* eof)
    & S.fromList
    & ( \ps ->
          [ a
            | x <- [1 .. 18],
              y <- [1 .. 18],
              z <- [0 .. 19],
              let a = exteriorArea (x, y, z) ps,
              isJust a,
              a /= Just 3454
          ]
      )
    & catMaybes
    & fmap (3454 -)
    & countMap
    & M.adjust (`div` 2) 10
    & M.adjust (`div` 1024) 1196
    & M.toList
    & fmap (uncurry (*))
    & sum
    & (3454 -)
