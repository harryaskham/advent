module TwentyFifteen.Day9 where

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

connections :: GenParser Char () (Map String [(String, Int)])
connections = M.fromListWith (++) . concat <$> many connection <* eof
  where
    connection = do
      l1 <- many1 letter
      string " to "
      l2 <- many letter
      string " = "
      d <- read <$> many1 digit
      eol
      return [(l1, [(l2, d)]), (l2, [(l1, d)])]

routeWith :: ([Maybe Int] -> Maybe Int) -> Map String [(String, Int)] -> Maybe Int
routeWith f cs = f (go 0 S.empty <$> M.keys cs)
  where
    go :: Int -> Set String -> String -> Maybe Int
    go d seen current
      | S.size seen == M.size cs - 1 = Just d
      | null nextStates = Nothing
      | otherwise =
        case filter isJust nextStates of
          [] -> Nothing
          ns -> f ns
      where
        nextStates =
          [ go (d + d') (S.insert current seen) neighbor
            | (neighbor, d') <- cs M.! current,
              not (neighbor `S.member` seen)
          ]

part1 :: IO (Maybe Int)
part1 = routeWith minimum . readWithParser connections <$> input 2015 9

part2 :: IO (Maybe Int)
part2 = routeWith maximum . readWithParser connections <$> input 2015 9
