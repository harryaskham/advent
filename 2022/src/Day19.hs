module Day19 (part1, part2) where

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

-- TODO: if we had the same number of robots or fewer but more ore then stop
quality :: Blueprint -> Int
quality blueprint = go (SQ.singleton (0, 0, 0, 0, 0, 1, 0, 0, 0)) M.empty 0
  where
    go SQ.Empty _ best = best
    go (st@(t, ore, clay, obsidian, geode, oreRs, clayRs, obsidianRs, geodeRs) SQ.:<| queue) seen best
      | t == 24 = go queue seen (max best (_id blueprint * geode))
      -- TODO: or if we had more robots in the past with less ore
      | cacheKey `M.member` seen && ore <= bestOre && clay <= bestClay && obsidian <= bestObsidian && geode <= bestGeode =
        go queue seen best
      | otherwise = traceShow (_id blueprint, best, st) $ go queue' seen' best
      where
        cacheKey = (oreRs, clayRs, obsidianRs, geodeRs)
        (bestOre, bestClay, bestObsidian, bestGeode) = seen M.! cacheKey
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
        -- If we can make a geode, just do that
        -- If we can make an obsidian this is the only way to use clay
        --next =
        --  catMaybes
        --    if isJust makeGeodeR
        --      then [makeGeodeR]
        --      else
        --        if isJust makeObsidianR
        --          then [makeObsidianR]
        --          else [makeNone, makeOreR, makeClayR, makeObsidianR, makeGeodeR]
        next = catMaybes [makeNone, makeOreR, makeClayR, makeObsidianR, makeGeodeR]
        queue' = queue SQ.>< SQ.fromList next
        seen' = M.insert cacheKey (ore, clay, obsidian, geode) seen

part1 :: Int
part1 =
  $(exampleInput 19)
    & parseWith parser
    & drop 1
    & take 1
    & fmap quality
    & sum

part2 :: Text
part2 = ""
