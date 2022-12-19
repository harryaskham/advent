module Day19 (part1, part2) where

import Data.Array qualified as A
import Data.Bimap (Bimap)
import Data.Bimap qualified as BM
import Data.List hiding (product, sum)
import Data.Map.Strict qualified as M
import Data.Mod
import Data.PQueue.Prio.Max qualified as PQ
import Data.Sequence qualified as SQ
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Read
import Data.Vector qualified as V
import Foreign.C (throwErrnoPath)
import Helper.Coord
import Helper.Grid
import Helper.TH
import Helper.Tracers
import Helper.Util
import Text.ParserCombinators.Parsec

data Blueprint = Blueprint
  { _id :: Int,
    _oreCost :: Int,
    _clayCost :: Int,
    _obsidianCost :: (Int, Int),
    _geodeCost :: (Int, Int)
  }
  deriving (Show)

parser :: Parser [Blueprint]
parser = many1 line <* eof
  where
    line = do
      (a, b, c, d, e, f, g) <- numberLine7
      return $ Blueprint a b c (d, e) (f, g)

potentialGeodes :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
potentialGeodes duration t obsidian obsidianRs geode geodeRs obsidianNeeded =
  let d = duration - t
      potentialObsidian = obsidian + d * obsidianRs + ((d * (d - 1)) `div` 2)
      newGeodesSupported = potentialObsidian `div` obsidianNeeded
      newGeodesPossible = d * geodeRs + ((d * (d - 1)) `div` 2)
   in -- geode + min newGeodesSupported newGeodesPossible
      geode + newGeodesPossible

geodesOpened :: Int -> Blueprint -> Int
geodesOpened duration blueprint = go (PQ.singleton (h init) init) M.empty 0
  where
    init = (0, 0, 0, 0, 0, 1, 0, 0, 0)
    h (t, _, _, obsidian, geode, _, _, obsidianRs, geodeRs) =
      potentialGeodes duration t obsidian obsidianRs geode geodeRs (snd (_geodeCost blueprint))
    go queue seen maxGeode
      | cacheKey `M.member` seen && ore <= bestOre && clay <= bestClay && obsidian <= bestObsidian && geode <= bestGeode =
        traceShow (_id blueprint, maxGeode, length queue, score, st, "cache hit, skipping") $
          go rest seen maxGeode
      | score <= maxGeode = maxGeode -- nothing in the queue can reach past the max ever so this must be a global max
      | otherwise =
        traceShow (_id blueprint, maxGeode, length queue, score, st) $
          go queue' seen' maxGeode'
      where
        ((score, st@(t, ore, clay, obsidian, geode, oreRs, clayRs, obsidianRs, geodeRs)), rest) = PQ.deleteFindMax queue
        cacheKey = (oreRs, clayRs, obsidianRs, geodeRs)
        (bestOre, bestClay, bestObsidian, bestGeode) = seen M.! cacheKey
        maxGeode' = max maxGeode geode' -- Store the highest seen incl. in the future
        t' = t + 1
        ore' = ore + oreRs
        clay' = clay + clayRs
        obsidian' = obsidian + obsidianRs
        geode' = geode + geodeRs
        makeNone = Just (t', ore', clay', obsidian', geode', oreRs, clayRs, obsidianRs, geodeRs)
        makeOreR =
          if ore >= _oreCost blueprint
            then Just (t', ore' - _oreCost blueprint, clay', obsidian', geode', oreRs + 1, clayRs, obsidianRs, geodeRs)
            else Nothing
        makeClayR =
          if ore >= _clayCost blueprint
            then Just (t', ore' - _clayCost blueprint, clay', obsidian', geode', oreRs, clayRs + 1, obsidianRs, geodeRs)
            else Nothing
        makeObsidianR =
          if ore >= fst (_obsidianCost blueprint) && clay >= snd (_obsidianCost blueprint)
            then Just (t', ore' - fst (_obsidianCost blueprint), clay' - snd (_obsidianCost blueprint), obsidian', geode', oreRs, clayRs, obsidianRs + 1, geodeRs)
            else Nothing
        makeGeodeR =
          if ore >= fst (_geodeCost blueprint) && obsidian >= snd (_geodeCost blueprint)
            then Just (t', ore' - fst (_geodeCost blueprint), clay', obsidian' - snd (_geodeCost blueprint), geode', oreRs, clayRs, obsidianRs, geodeRs + 1)
            else Nothing
        next = catMaybes [makeNone, makeOreR, makeClayR, makeObsidianR, makeGeodeR]
        queue' = foldl' (\q st -> PQ.insert (h st) st q) rest next
        seen' = M.insert cacheKey (ore, clay, obsidian, geode) seen

part1 :: Int
part1 =
  $(exampleInput 19)
    & parseWith parser
    & fmap (\b -> _id b * geodesOpened 24 b)
    & sum

part2 :: Int
part2 =
  $(input 19)
    & parseWith parser
    & take 3
    & (\a -> (!!) a <$> [0, 2])
    & fmap (geodesOpened 32)
    & product

-- 288 too low
