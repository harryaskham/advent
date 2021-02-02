module TwentyEighteen.Day25 where

import Control.Monad
import Control.Monad.Memo
import Coord
import Data.Bits
import Data.Char
import qualified Data.Foldable as F
import Data.Function
import Data.List
import Data.List.Extra
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Sequence (Seq)
import qualified Data.Sequence as SQ
import Data.Set (Set)
import qualified Data.Set as S
import Data.Tuple.Extra
import Data.Vector (Vector)
import qualified Data.Vector as V
import Debug.Trace
import Grid
import Text.ParserCombinators.Parsec
import Util

coords :: GenParser Char () [Coord4]
coords = do
  cs <- many1 coord
  eof
  return cs
  where
    num = read <$> many1 (oneOf "-0123456789")
    coord = do
      [x, y, z, w] <- num `sepBy` char ','
      eol
      return (x, y, z, w)

constellations :: Map Coord4 [Coord4] -> [[Coord4]]
constellations conn = go (M.keys conn) S.empty []
  where
    go [] _ constellations = constellations
    go (c : cs) seen constellations
      | c `S.member` seen = go cs seen constellations
      | otherwise = go cs nextSeen nextConstellations
      where
        constellation = constellationFrom (SQ.singleton c) S.empty
        nextSeen = foldl' (flip S.insert) seen constellation
        nextConstellations = constellation : constellations
    constellationFrom SQ.Empty seen = S.toList seen
    constellationFrom (c SQ.:<| queue) seen
      | c `S.member` seen = constellationFrom queue seen
      | otherwise = constellationFrom (queue SQ.>< SQ.fromList (conn M.! c)) (S.insert c seen)

part1 :: IO Int
part1 = do
  cs <- readWithParser coords <$> input 2018 25
  let connections = M.fromList [(c, [c' | c' <- cs, c' /= c, manhattan4 c c' <= 3]) | c <- cs]
  return . length . constellations $ connections
