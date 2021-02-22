module TwentyFifteen.Day19 where

import Data.List (foldl', nub)
import Data.Map (Map)
import qualified Data.Map.Strict as M
import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.Set as S
import Text.ParserCombinators.Parsec
  ( GenParser,
    eof,
    letter,
    many,
    many1,
    string,
  )
import Util (eol, input, readWithParser, replaceOnes)

type Replacement = (String, String)

replacements :: GenParser Char () (Map String [String], String)
replacements = do
  rs <- many (replacement <* eol)
  eol
  target <- many1 letter <* eol
  eof
  return (M.fromListWith (++) rs, target)
  where
    replacement = do
      a <- many1 letter
      string " => "
      b <- many1 letter
      return (a, [b])

singleReplacements :: Map String [String] -> String -> [String]
singleReplacements rs xs =
  nub . concat $
    [ replaceOnes old new xs
      | (old, news) <- M.toList rs,
        new <- news
    ]

part1 :: IO Int
part1 = do
  (rs, target) <- readWithParser replacements <$> input 2015 19
  return . length $ singleReplacements rs target

shortestConstruction :: Map String [String] -> String -> Maybe Int
shortestConstruction rs' target =
  go (PQ.singleton (length target) (target, 0)) S.empty
  where
    rs = M.fromListWith (++) [(b, [a]) | (a, bs) <- M.toList rs', b <- bs]
    go queue seen
      | null queue = Nothing
      | current == "e" = Just steps
      | current `S.member` seen = go rest seen
      | otherwise = go nextQueue (S.insert current seen)
      where
        ((_, (current, steps)), rest) = PQ.deleteFindMin queue
        nextStates =
          [ (length s `div` 2, (s, steps + 1))
            | s <- singleReplacements rs current
          ]
        nextQueue = foldl' (\q (h, v) -> PQ.insert h v q) rest nextStates

part2 :: IO (Maybe Int)
part2 = do
  (rs, target) <- readWithParser replacements <$> input 2015 19
  return $ shortestConstruction rs target
