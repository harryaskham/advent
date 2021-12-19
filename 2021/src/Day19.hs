module Day19 (part1, part2) where

import Control.Arrow ((***))
import Control.Lens (over)
import Data.Foldable (foldl1, foldr)
import Data.List qualified as L
import Data.List.Extra (maximumOn)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Helper.Coord (manhattan3)
import Helper.TH (input)
import Helper.Util (eol, fromV3, iterateFix, nSameIn, number, parseWith, permsV3, powerset, toTuple3, toV3)
import Linear.V3 (R1 (_x), R2 (_y), R3 (_z), V3 (..))
import Safe (headMay)
import Text.ParserCombinators.Parsec (GenParser, between, char, eof, many1, sepBy1, string)

data Scanner = Scanner {scannerID :: Int, beacons :: Set (V3 Int)} deriving (Eq, Ord)

instance Semigroup Scanner where
  (Scanner i a) <> (Scanner _ b) = Scanner i (a `S.union` b)

scanner :: GenParser Char () Scanner
scanner =
  Scanner
    <$> (between (string "--- scanner ") (string " ---") number <* eol)
    <*> (S.fromList <$> (many1 (toV3 . toTuple3 <$> number `sepBy1` char ',' <* eol) <* optional eol))

orientations :: Scanner -> Set Scanner
orientations (Scanner i bs) = S.fromList $ Scanner i <$> [S.map t bs | t <- transforms]
  where
    negations = filter (not . null) (powerset [id, over _x negate, over _y negate, over _z negate])
    transforms = (.) <$> (foldl1 (.) <$> negations) <*> permsV3

reorient :: Scanner -> Scanner -> Maybe (Scanner, V3 Int)
reorient (Scanner _ bs0) (Scanner i bs) =
  (Scanner i . flip S.map bs . subtract &&& id)
    <$> nSameIn 12 [a - b | a <- S.toList bs, b <- S.toList bs0]

mergeMany :: ((Scanner, [V3 Int]), Map Int Scanner) -> ((Scanner, [V3 Int]), Map Int Scanner)
mergeMany ((s0, ps), ss) = foldr mergeMatch ((s0, ps), ss) (matches (M.elems ss))
  where
    mergeMatch (s, p) = ((<> s) *** (p :)) *** M.delete (scannerID s)
    matches [] = []
    matches (s : ss) =
      case mapMaybe (reorient s0) (S.toList (orientations s)) of
        (m : _) -> m : matches ss
        [] -> matches ss

normalizedScanners :: (Scanner, [V3 Int])
normalizedScanners =
  let ss = parseWith (many1 scanner <* eof) $(input 19)
   in fst . iterateFix mergeMany $
        ((L.head ss, [V3 0 0 0]), M.fromList [(scannerID s, s) | s <- L.tail ss])

part1 :: Int
part1 = S.size . beacons . fst $ normalizedScanners

part2 :: Int
part2 =
  L.maximum
    [ manhattan3 (fromV3 a) (fromV3 b)
      | let ps = snd normalizedScanners,
        a <- ps,
        b <- ps,
        a /= b
    ]
