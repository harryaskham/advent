module Day4 (part1, part2) where

import Data.Array qualified as A
import Data.Bimap (Bimap)
import Data.Bimap qualified as BM
import Data.Map.Strict qualified as M
import Data.Mod
import Data.PQueue.Prio.Min qualified as PQ
import Data.Sequence qualified as SQ
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Read
import Data.Vector qualified as V
import Helper.Coord
import Helper.Grid
import Helper.Tracers
import Helper.Util
import Text.ParserCombinators.Parsec
import Prelude hiding (many)

type Call = Int

data Board = Board (Map Int (Int, Int)) (Set (Int, Int))

toBoard :: [[Int]] -> Board
toBoard ass =
  Board (M.fromList [(a, (x, y)) | (y, as) <- zip [0 ..] ass, (x, a) <- zip [0 ..] as]) S.empty

parser :: GenParser Char () ([Call], [Board])
parser = do
  calls <- number `sepBy` char ',' <* (eol >> eol)
  let boardLine = many (char ' ') >> many1 (number <* many (char ' '))
      board = many1 (boardLine <* eol)
  boards <- (toBoard <$$> board `sepBy` eol) <* eof
  return (calls, boards)

mark :: Int -> Board -> Board
mark x b@(Board m marked) =
  case M.lookup x m of
    Nothing -> b
    Just p -> Board m (S.insert p marked)

isWon :: Board -> Bool
isWon (Board _ marked) = any isRowComplete rows
  where
    rows = uncurry (++) . (id &&& transpose) $ [[(x, y) | y <- [0 .. 4]] | x <- [0 .. 4]]
    isRowComplete = all (`S.member` marked)

firstWinningBoard :: [Int] -> [Board] -> Maybe (Board, Int)
firstWinningBoard (c : calls) boards =
  let boards' = mark c <$> boards
   in case filter isWon boards' of
        [] -> firstWinningBoard calls boards'
        b : _ -> Just (b, c)
firstWinningBoard _ _ = Nothing

score :: Board -> Int
score (Board m marked) = sum [a | (a, p) <- M.toList m, not (p `S.member` marked)]

solve :: ([Call] -> [Board] -> Maybe (Board, Int)) -> IO (Maybe Int)
solve f = do
  (calls, boards) <- parseInput parser (input 4)
  return $ uncurry (*) . first score <$> f calls boards

part1 :: IO (Maybe Int)
part1 = solve firstWinningBoard

lastWinningBoard :: Maybe (Board, Int) -> [Int] -> [Board] -> Maybe (Board, Int)
lastWinningBoard lastWon [] _ = lastWon
lastWinningBoard lastWon (c : calls) boards =
  let boards' = mark c <$> boards
   in case filter isWon boards' of
        b : _ -> lastWinningBoard (Just (b, c)) calls (filter (not . isWon) boards')
        _ -> lastWinningBoard lastWon calls (filter (not . isWon) boards')

part2 :: IO (Maybe Int)
part2 = solve (lastWinningBoard Nothing)
