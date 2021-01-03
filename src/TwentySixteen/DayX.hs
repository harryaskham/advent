module TwentySixteen.DayX where

import Data.Char
import qualified Data.Foldable as F
import Data.Function
import Data.List
import Data.List.Extra
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Sequence (Seq)
import qualified Data.Sequence as SQ
import Data.Set (Set)
import qualified Data.Set as S
import Data.Tuple.Extra
import Data.Vector (Vector)
import qualified Data.Vector as V
import Debug.Trace
import Text.ParserCombinators.Parsec
import Util

part1 :: IO Int
part1 = do
  ls <- lines <$> input 2016 11
  print ls
  return 0
