module Day19 where

import Data.Foldable (foldl1)
import Data.List ((!!))
import Data.List qualified as L
import Data.List.Extra (maximumOn)
import Data.Map.Strict qualified as M
import Data.Mod ()
import Data.PQueue.Prio.Min qualified as PQ
import Data.Sequence qualified as SQ
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Read ()
import Data.Tuple.Extra (first3, second3, third3)
import Data.Vector qualified as V
import Helper.Coord (manhattan3)
import Helper.Grid ()
import Helper.TH (input)
import Helper.Tracers (traceShowF)
import Helper.Util
  ( countMap,
    eol,
    number,
    parseWith,
    perms3,
    powerset,
    toList3,
    toTuple3,
  )
import Text.ParserCombinators.Parsec
  ( GenParser,
    char,
    eof,
    many1,
    sepBy1,
    string,
  )

data Scanner = Scanner Int [(Int, Int, Int)] deriving (Eq, Ord, Show)

parser :: GenParser Char () [Scanner]
parser = many1 scanner <* eof
  where
    scanner = do
      string "--- scanner "
      sID <- number
      string " ---"
      eol
      beacons <- many1 ((toTuple3 <$> number `sepBy1` char ',') <* eol)
      optional eol
      return $ Scanner sID beacons

vary :: Scanner -> [Scanner]
vary (Scanner i bs) = L.nub $ Scanner i <$> [t . p <$> bs | t <- transforms, p <- perms3]
  where
    transforms =
      foldl1 (.)
        <$> filter
          (not . null)
          (powerset [id, first3 negate, second3 negate, third3 negate])

-- If this scanner overlaps with a zero oriented scanner, then keep its orientation, and shift its reference frame to zero
overlap :: Scanner -> Scanner -> Maybe (Scanner, (Int, Int, Int))
overlap s@(Scanner i bs) s0@(Scanner _ bs0)
  | diffCount >= 12 =
    traceShow ("found overlap", maxDiff) $
      Just (Scanner i (sub3 <$> bs <*> pure maxDiff), maxDiff)
  | otherwise = Nothing
  where
    sub3 (x, y, z) (x', y', z') = (x - x', y - y', z - z')
    (maxDiff, diffCount) = maximumOn snd . M.toList $ countMap $ sub3 <$> bs <*> bs0

-- start with zero
-- find one that overlaps and call s0 (0,0)

getID :: Scanner -> Int
getID (Scanner i _) = i

addOne :: [(Scanner, (Int, Int, Int))] -> [Scanner] -> ([(Scanner, (Int, Int, Int))], [Scanner])
addOne zeroScanners scanners =
  let (s@(Scanner i _), pos) = go scanners
   in ((s, pos) : zeroScanners, filter ((/= i) . getID) scanners)
  where
    go (scanner : scanners)
      | null overlaps = go scanners
      | otherwise = overlaps !! 0
      where
        overlaps = catMaybes [overlap s s0 | s <- vary scanner, (s0, _) <- zeroScanners]

addAll :: [(Scanner, (Int, Int, Int))] -> [Scanner] -> [(Scanner, (Int, Int, Int))]
addAll zeroScanners [] = zeroScanners
addAll zeroScanners scanners = uncurry addAll (addOne zeroScanners scanners)

allPoints :: [Scanner] -> [(Int, Int, Int)]
allPoints scanners = L.nub $ foldl' (\bss (Scanner _ bs) -> bss ++ bs) [] scanners

normalizedScanners :: [(Scanner, (Int, Int, Int))]
normalizedScanners =
  let (s0 : scanners) = $(input 19) & parseWith parser
   in addAll [(s0, (0, 0, 0))] scanners

part1 :: Int
part1 = length (allPoints (fst <$> normalizedScanners))

part2 :: Int
part2 =
  let ps = snd <$> normalizedScanners
   in L.maximum [manhattan3 a b | a <- ps, b <- ps, a /= b]
