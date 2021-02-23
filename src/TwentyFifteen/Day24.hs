module TwentyFifteen.Day24 where

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

-- TODO: symmetry of second two groups, can key seen better
-- maybe a PQ? keyed by number allocated, size of first group, quantum number

allocate :: [Int] -> Maybe (Set Int, Set Int, Set Int)
allocate xs = go (SQ.singleton (reverse xs, S.empty, S.empty, S.empty)) Nothing
  where
    go SQ.Empty best = best
    go ((remaining, g1, g2, g3) SQ.:<| queue) best
      | null remaining = go queue nextBest
      | isJust best && (Just $ S.size g1) > (S.size . fst3 <$> best) = go queue best
      | sr < abs (sg2 - sg1) || sr < abs (sg3 - sg2) || sr < abs (sg3 - sg1) = go queue best
      -- | (g1, g2, g3) `S.member` seen = traceShow "seen" $ go queue seen best
      -- | (g1, g3, g2) `S.member` seen = traceShow "seen" $ go queue seen best
      | otherwise =
        traceShow  best $
           go (nextStates SQ.>< queue) best
          --go (queue SQ.>< nextStates) best
      where
        (sr, sg1, sg2, sg3) = (sum remaining, sum g1, sum g2, sum g3)
        nextBest = case best of
          Nothing ->
            if sg1 == sg2 && sg2 == sg3
              then Just (g1, g2, g3)
              else Nothing
          Just (bg1, _, _) ->
            if sg1 == sg2 && sg2 == sg3
              then
                if (S.size g1 < S.size bg1) || (S.size g1 == S.size bg1 && product g1 < product bg1)
                  then Just (g1, g2, g3)
                  else best
              else best
        (r : rs) = remaining
        nextStates =
          SQ.fromList [
              (rs, g1, S.insert r g2, g3),
              (rs, g1, g2, S.insert r g3),
              (rs, S.insert r g1, g2, g3)]

-- 317793762227 too highi

part1 :: IO Int
part1 = product . fst3 . unjust . allocate . fmap read . lines <$> input 2015 24
