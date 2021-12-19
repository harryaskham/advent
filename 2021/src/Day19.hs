module Day19 where

import Data.Foldable (foldl1)
import Data.List qualified as L
import Data.List.Extra (maximumOn)
import Data.Map.Strict qualified as M
import Data.Tuple.Extra (first3, second3, third3)
import Helper.Coord (manhattan3)
import Helper.TH (input)
import Helper.Util (countMap, eol, number, parseWith, perms3, powerset, toTuple3)
import Safe (headMay)
import Text.ParserCombinators.Parsec (GenParser, between, char, eof, many1, sepBy1, string)

data Scanner = Scanner
  { scannerID :: Int,
    beacons :: [(Int, Int, Int)]
  }
  deriving (Eq, Ord, Show)

scanner :: GenParser Char () Scanner
scanner =
  Scanner
    <$> (between (string "--- scanner ") (string " ---") number <* eol)
    <*> (many1 ((toTuple3 <$> number `sepBy1` char ',') <* eol) <* optional eol)

parser :: GenParser Char () [Scanner]
parser = many1 scanner <* eof

reorient :: Scanner -> [Scanner]
reorient (Scanner i bs) = L.nub $ Scanner i <$> [t . p <$> bs | t <- transforms, p <- perms3]
  where
    transforms =
      foldl1 (.)
        <$> filter (not . null) (powerset [id, first3 negate, second3 negate, third3 negate])

overlap :: Scanner -> Scanner -> Maybe (Scanner, (Int, Int, Int))
overlap (Scanner i bs) (Scanner _ bs0)
  | diffCount >= 12 = Just (Scanner i (sub3 <$> bs <*> pure maxDiff), maxDiff)
  | otherwise = Nothing
  where
    sub3 (x, y, z) (x', y', z') = (x - x', y - y', z - z')
    (maxDiff, diffCount) = maximumOn snd . M.toList $ countMap $ sub3 <$> bs <*> bs0

addOne :: [(Scanner, (Int, Int, Int))] -> [Scanner] -> ([(Scanner, (Int, Int, Int))], [Scanner])
addOne scanners0 scanners =
  case go scanners of
    Nothing -> (scanners0, scanners)
    Just (s, pos) -> ((s, pos) : scanners0, filter ((/= scannerID s) . scannerID) scanners)
  where
    go [] = Nothing
    go (scanner : scanners) =
      headMay (catMaybes [overlap s s0 | s <- reorient scanner, (s0, _) <- scanners0])
        <|> go scanners

addAll :: [(Scanner, (Int, Int, Int))] -> [Scanner] -> [(Scanner, (Int, Int, Int))]
addAll scanners0 [] = scanners0
addAll scanners0 scanners = uncurry addAll (addOne scanners0 scanners)

allPoints :: [Scanner] -> [(Int, Int, Int)]
allPoints = L.nub . concatMap beacons

normalizedScanners :: [(Scanner, (Int, Int, Int))]
normalizedScanners =
  let scanners = $(input 19) & parseWith parser
   in addAll [(L.head scanners, (0, 0, 0))] (L.tail scanners)

part1 :: Int
part1 = length (allPoints (fst <$> normalizedScanners))

part2 :: Int
part2 =
  let ps = snd <$> normalizedScanners
   in L.maximum [manhattan3 a b | a <- ps, b <- ps, a /= b]
