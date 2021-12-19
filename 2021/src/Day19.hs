module Day19 where

import Data.Array qualified as A
import Data.Bimap (Bimap)
import Data.Bimap qualified as BM
import Data.Foldable (foldl1)
import Data.List (delete, nub, (!!))
import Data.List.Extra (maximumOn)
import Data.Map.Strict qualified as M
import Data.Mod
import Data.PQueue.Prio.Min qualified as PQ
import Data.Sequence qualified as SQ
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Read
import Data.Tuple.Extra
import Data.Vector qualified as V
import Helper.Coord
import Helper.Grid
import Helper.TH
import Helper.Tracers
import Helper.Util
import Text.ParserCombinators.Parsec hiding (optional)
import Data.List qualified as L

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
vary (Scanner i bs) = traceShowF length $ nub $ Scanner i <$> bss
  where
    perms =
      [ \(x, y, z) -> (x, y, z),
        \(x, y, z) -> (x, z, y),
        \(x, y, z) -> (y, x, z),
        \(x, y, z) -> (y, z, x),
        \(x, y, z) -> (z, x, y),
        \(x, y, z) -> (z, y, x)
      ]
    negateX = first3 negate
    negateY = second3 negate
    negateZ = third3 negate
    transforms = (foldl1 (.) <$> filter ((> 0) . length) (powerset [id, negateX, negateY, negateZ]))
    bss = [t . p <$> bs | t <- transforms, p <- perms]

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

getId (Scanner i _) = i

addOne :: [(Scanner, (Int, Int, Int))] -> [Scanner] -> ([(Scanner, (Int ,Int, Int))], [Scanner])
addOne zeroScanners scanners =
  let (s@(Scanner i _), pos) = go scanners
   in ((s, pos) : zeroScanners, filter ((/= i) . getId) scanners)
  where
    go (scanner : scanners)
      -- | length overlaps /= length (nub overlaps) = error "ambiuous scan"
      | null overlaps = traceShow "could not find any overlaps" $ go scanners
      | otherwise = overlaps !! 0
      where
        overlaps = catMaybes [overlap s s0 | s <- vary scanner, (s0,_) <- zeroScanners]

addAll zeroScanners [] = zeroScanners
addAll zeroScanners scanners = uncurry addAll (addOne zeroScanners scanners)

allPoints :: [Scanner] -> [(Int, Int, Int)]
allPoints scanners = nub $ foldl' (\bss (Scanner _ bs) -> bss ++ bs) [] scanners

part1 :: Int
part1 =
  let (s0 : scanners) = $(input 19) & parseWith parser
   in length (allPoints (fst <$> addAll [(s0, (0, 0, 0))] scanners))

part2 :: Int
part2 =
  let (s0 : scanners) = $(input 19) & parseWith parser
      ps = snd <$> addAll [(s0, (0, 0, 0))] scanners
   in L.maximum [manhattan3 a b | a <- ps, b <- ps, a /= b]
