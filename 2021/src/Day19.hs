module Day19 (part1, part2) where

import Control.Lens (over)
import Data.Foldable (foldl1)
import Data.List qualified as L
import Data.List.Extra (maximumOn)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Helper.Coord (manhattan3)
import Helper.TH (input)
import Helper.Util (eol, fromV3, nSameIn, number, parseWith, permsV3, powerset, toTuple3, toV3)
import Linear.V3 (R1 (_x), R2 (_y), R3 (_z), V3 (..))
import Safe (headMay)
import Text.ParserCombinators.Parsec (GenParser, between, char, eof, many1, sepBy1, string)

data Scanner = Scanner {scannerID :: Int, beacons :: [V3 Int]} deriving (Eq, Ord)

scanner :: GenParser Char () Scanner
scanner =
  Scanner
    <$> (between (string "--- scanner ") (string " ---") number <* eol)
    <*> (many1 (toV3 . toTuple3 <$> number `sepBy1` char ',' <* eol) <* optional eol)

orientations :: Scanner -> Set Scanner
orientations (Scanner i bs) = S.fromList $ Scanner i <$> [t <$> bs | t <- transforms]
  where
    negations' = filter (not . null) (powerset [id, over _x negate, over _y negate, over _z negate])
    negations = foldl1 (.) <$> negations'
    transforms = (.) <$> negations <*> permsV3

reorient :: Scanner -> Scanner -> Maybe (Scanner, V3 Int)
reorient (Scanner _ bs0) (Scanner i bs) =
  (Scanner i . flip fmap bs . subtract &&& id)
    <$> nSameIn 12 [a - b | a <- bs, b <- bs0]

mergeOne :: ((Scanner, [V3 Int]), Map Int Scanner) -> ((Scanner, [V3 Int]), Map Int Scanner)
mergeOne ((s0, ps), ss) = ((merge s0 s, p : ps), M.delete (scannerID s) ss)
  where
    merge (Scanner i bs) (Scanner _ bs') = Scanner i (L.nub $ bs ++ bs')
    firstMatch (s : ss) =
      case mapMaybe (reorient s0) (S.toList (orientations s)) of
        (m : _) -> m
        [] -> firstMatch ss
    (s, p) = firstMatch (M.elems ss)

normalizedScanners :: (Scanner, [V3 Int])
normalizedScanners =
  let ss = $(input 19) & parseWith (many1 scanner <* eof)
   in iterate mergeOne ((L.head ss, [V3 0 0 0]), M.fromList [(scannerID s, s) | s <- L.tail ss])
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
