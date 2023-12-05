module Day5 (part1, part2) where

import Data.Array qualified as A
import Data.Bimap (Bimap)
import Data.Bimap qualified as BM
import Data.List (foldl1, foldr1, minimum)
import Data.List.Extra (chunksOf)
import Data.Map.Strict qualified as M
import Data.Mod
import Data.PQueue.Prio.Min qualified as PQ
import Data.Sequence qualified as SQ
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Read
import Data.Tuple.Extra (fst3, snd3)
import Data.Vector qualified as V
import Helper.Coord
import Helper.Grid
import Helper.TH
import Helper.Tracers
import Helper.Util
import Relude.Unsafe qualified as U
import Text.ParserCombinators.Parsec

parser :: Parser ([Integer], [Integer], [(Integer -> Integer, Integer -> Integer, Integer -> Integer)])
parser = do
  string "seeds: "
  seeds <- (number `sepBy1` string " ") <* (eol >> eol)
  (mappings, destinations) <- unzip <$> (mapping `sepBy1` eol) <* eof
  return (seeds, concat destinations, mappings)
  where
    rangeLine = do
      destinationStart <- number <* char ' '
      sourceStart <- number <* char ' '
      rangeSize <- number <* eol
      return $
        ( ( \x ->
              if (x >= sourceStart) && (x < sourceStart + rangeSize)
                then Just $ destinationStart + (x - sourceStart)
                else Nothing,
            \x ->
              if (x >= destinationStart) && (x < destinationStart + rangeSize)
                then Just $ sourceStart + (x - destinationStart)
                else Nothing
          ),
          destinationStart
        )
    mapping = do
      many1 (noneOf "\n") >> eol
      -- (traceShowId .) . foldl1 (.) <$> many1 rangeLine
      (fs, destinations) <- unzip <$> many1 rangeLine
      let forwardBackward =
            toTuple2 $
              ( \f x -> case catMaybes (f <$> fs <*> pure x) of
                  [] -> x
                  (x' : _) -> x'
              )
                <$> [fst, snd]
      -- remove the third item
      return ((fst forwardBackward, snd forwardBackward, snd forwardBackward), destinations) -- , snd <$> fs)

part1 :: Integer
part1 =
  -- \ $(exampleInput 5)
  $(input 5)
    & parseWith parser
    & ( \(seeds, _, mappings) ->
          let pipeline x = scanl' (flip fst3) x mappings
           in U.last . pipeline <$> seeds
      )
    & minimum

-- we can do binary search on the locations  to find one that is in range
part2 :: Integer
part2 =
  -- \ $(exampleInput 5)
  $(input 5)
    & parseWith parser
    & ( \(seeds, destinations, mappings) ->
          let pipeline x = scanr snd3 x mappings
              validSeed seed = or [(seed >= a) && (seed < a + b) | [a, b] <- chunksOf 2 seeds]
              t = 1
           in [location | d <- destinations, location <- [d - t .. d + t], validSeed (U.head $ pipeline $ traceShowId location)]
      )
    & minimum
