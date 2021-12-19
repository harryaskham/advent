module Day19 (part1, part2) where

import Control.Lens (over)
import Data.Foldable (foldl1)
import Data.List qualified as L
import Data.List.Extra (maximumOn)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Helper.Coord (manhattan3)
import Helper.TH (input)
import Helper.Util (adjustWithDefault, countMap, eol, fromV3, nSameIn, number, parseWith, permsV3, powerset, toTuple3, toV3)
import Linear.V3 (R1 (_x), R2 (_y), R3 (_z), V3 (..))
import Safe (headMay)
import Text.ParserCombinators.Parsec (GenParser, between, char, eof, many1, sepBy1, string)

data Scanner = Scanner {scannerID :: Int, beacons :: [V3 Int]} deriving (Eq, Ord)

scanner :: GenParser Char () Scanner
scanner =
  Scanner
    <$> (between (string "--- scanner ") (string " ---") number <* eol)
    <*> (many1 ((toV3 . toTuple3 <$> number `sepBy1` char ',') <* eol) <* optional eol)

orientations :: Scanner -> Set Scanner
orientations (Scanner i bs) =
  S.fromList $ Scanner i <$> [t . p <$> bs | t <- transforms, p <- permsV3]
  where
    transforms =
      foldl1 (.)
        <$> filter
          (not . null)
          (powerset [id, over _x negate, over _y negate, over _z negate])

reorient :: Scanner -> Scanner -> Maybe (Scanner, V3 Int)
reorient (Scanner _ bs0) (Scanner i bs) =
  case nSameIn 12 ((-) <$> bs <*> bs0) of
    Just diff -> Just (Scanner i (subtract diff <$> bs), diff)
    Nothing -> Nothing

mergeOne :: ((Scanner, [V3 Int]), [Scanner]) -> ((Scanner, [V3 Int]), [Scanner])
mergeOne ((s0, ps), ss) =
  let (s, p) = firstMatch ss
   in ((merge s0 s, p : ps), filter ((/= scannerID s) . scannerID) ss)
  where
    merge (Scanner i bs) (Scanner _ bs') = Scanner i (L.nub $ bs ++ bs')
    firstMatch (s : ss) =
      case (mapMaybe (reorient s0) (S.toList (orientations s))) of
        (m : _) -> m
        [] -> firstMatch ss

normalizedScanners :: (Scanner, [V3 Int])
normalizedScanners =
  let ss = $(input 19) & parseWith (many1 scanner <* eof)
   in iterate mergeOne ((L.head ss, [V3 0 0 0]), L.tail ss)
        & dropWhile (not . null . snd)
        & L.head
        & fst

part1 :: Int
part1 = length (L.nub . beacons $ fst normalizedScanners)

part2 :: Int
part2 =
  L.maximum
    [ manhattan3 (fromV3 a) (fromV3 b)
      | let ps = snd normalizedScanners,
        a <- ps,
        b <- ps,
        a /= b
    ]
