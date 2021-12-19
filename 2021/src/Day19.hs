module Day19 where

import Control.Lens (over)
import Data.Foldable (foldl1)
import Data.List qualified as L
import Data.List.Extra (maximumOn)
import Data.Map.Strict qualified as M
import Helper.Coord (manhattan3)
import Helper.TH (input)
import Helper.Util (countMap, eol, fromV3, number, parseWith, perms3, permsV3, powerset, toTuple3, toV3)
import Linear.V3 (R1 (_x), R2 (_y), R3 (_z), V3 (..))
import Safe (headMay)
import Text.ParserCombinators.Parsec (GenParser, between, char, eof, many1, sepBy1, string)

data Scanner = Scanner
  { scannerID :: Int,
    beacons :: [V3 Int]
  }
  deriving (Eq, Ord, Show)

scanner :: GenParser Char () Scanner
scanner =
  Scanner
    <$> (between (string "--- scanner ") (string " ---") number <* eol)
    <*> (many1 ((toV3 . toTuple3 <$> number `sepBy1` char ',') <* eol) <* optional eol)

reorient :: Scanner -> [Scanner]
reorient (Scanner i bs) = L.nub $ Scanner i <$> [t . p <$> bs | t <- transforms, p <- permsV3]
  where
    transforms =
      foldl1 (.)
        <$> filter
          (not . null)
          (powerset [id, over _x negate, over _y negate, over _z negate])

overlap :: Scanner -> Scanner -> Maybe (Scanner, V3 Int)
overlap (Scanner i bs) (Scanner _ bs0)
  | diffCount >= 12 = Just (Scanner i ((-) <$> bs <*> pure maxDiff), maxDiff)
  | otherwise = Nothing
  where
    (maxDiff, diffCount) = maximumOn snd . M.toList $ countMap $ (-) <$> bs <*> bs0

addOne :: [(Scanner, V3 Int)] -> [Scanner] -> ([(Scanner, V3 Int)], [Scanner])
addOne ss0 ss =
  case firstMatch ss of
    Nothing -> (ss0, ss)
    Just (s, pos) -> ((s, pos) : ss0, filter ((/= scannerID s) . scannerID) ss)
  where
    firstMatch [] = Nothing
    firstMatch (s : ss) =
      headMay
        (catMaybes [overlap s' s0 | s' <- reorient s, (s0, _) <- ss0])
        <|> firstMatch ss

addAll :: [(Scanner, V3 Int)] -> [Scanner] -> [(Scanner, V3 Int)]
addAll scanners0 [] = scanners0
addAll scanners0 scanners = uncurry addAll (addOne scanners0 scanners)

normalizedScanners :: [(Scanner, V3 Int)]
normalizedScanners =
  let scanners = $(input 19) & parseWith (many1 scanner <* eof)
   in addAll [(L.head scanners, V3 0 0 0)] (L.tail scanners)

part1 :: Int
part1 = length (L.nub . concatMap beacons $ (fst <$> normalizedScanners))

part2 :: Int
part2 =
  L.maximum
    [ manhattan3 (fromV3 a) (fromV3 b)
      | let ps = snd <$> normalizedScanners,
        a <- ps,
        b <- ps,
        a /= b
    ]
