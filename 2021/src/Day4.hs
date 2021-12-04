module Day4 (part1, part2) where

import Data.IntMap qualified as IM
import Data.Set qualified as S
import Helper.Util (eol, input, number, parseInput, (<$$>))
import Text.ParserCombinators.Parsec (GenParser, char, eof, many1, sepBy)

data Board = Board (IntMap (Int, Int)) (Set (Int, Int))

toBoard :: [[Int]] -> Board
toBoard ass =
  Board
    ( IM.fromList
        [ (a, (x, y))
          | (y, as) <- zip [0 ..] ass,
            (x, a) <- zip [0 ..] as
        ]
    )
    S.empty

parser :: GenParser Char () ([Int], [Board])
parser = do
  calls <- number `sepBy` char ',' <* (eol >> eol)
  let boardLine = many (char ' ') >> many1 (number <* many (char ' '))
      board = many1 (boardLine <* eol)
  boards <- (toBoard <$$> board `sepBy` eol) <* eof
  return (calls, boards)

mark :: Int -> Board -> Board
mark a b@(Board m marked) =
  maybe b (Board m) ((`S.insert` marked) <$> m IM.!? a)

isWon :: Board -> Bool
isWon (Board _ marked) =
  any
    (all (`S.member` marked))
    ( uncurry (++) . (id &&& transpose) $
        [[(x, y) | y <- [0 .. 4]] | x <- [0 .. 4]]
    )

score :: Board -> Int
score (Board m marked) =
  sum [a | (a, p) <- IM.toList m, not (p `S.member` marked)]

firstWinningBoard :: [Int] -> [Board] -> Maybe (Board, Int)
firstWinningBoard (c : calls) boards =
  let markedBoards = mark c <$> boards
   in case filter isWon markedBoards of
        [] -> firstWinningBoard calls markedBoards
        b : _ -> Just (b, c)
firstWinningBoard _ _ = Nothing

lastWinningBoard :: Maybe (Board, Int) -> [Int] -> [Board] -> Maybe (Board, Int)
lastWinningBoard lastWon [] _ = lastWon
lastWinningBoard lastWon (c : calls) boards =
  let markedBoards = mark c <$> boards
      remainingBoards = filter (not . isWon) markedBoards
      lastWon' = case filter isWon markedBoards of
        b : _ -> Just (b, c)
        _ -> lastWon
   in lastWinningBoard lastWon' calls remainingBoards

scoreBoard :: ([Int] -> [Board] -> Maybe (Board, Int)) -> IO (Maybe Int)
scoreBoard f =
  fmap (uncurry (*) . first score) . uncurry f
    <$> parseInput parser (input 4)

part1 :: IO (Maybe Int)
part1 = scoreBoard firstWinningBoard

part2 :: IO (Maybe Int)
part2 = scoreBoard (lastWinningBoard Nothing)
