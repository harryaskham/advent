module Day21 (part1, part2) where

import Control.Monad.Memo (Memo, MonadMemo (memo), startEvalMemo)

data Game = Game
  { p1Pos :: Int,
    p2Pos :: Int,
    p1Score :: Int,
    p2Score :: Int,
    p1Turn :: Bool
  }
  deriving (Eq, Ord, Show)

mkGame :: Int -> Int -> Game
mkGame p1Pos p2Pos = Game (p1Pos - 1) (p2Pos - 1) 0 0 True

run :: [Int] -> Int -> Game -> Int
run rolls numRolls game@Game {..}
  | p1Score >= 1000 = p2Score * numRolls
  | p2Score >= 1000 = p1Score * numRolls
  | p1Turn =
    run rolls' numRolls' $
      game {p1Pos = p1Pos', p1Score = p1Score', p1Turn = p1Turn'}
  | otherwise =
    run rolls' numRolls' $
      game {p2Pos = p2Pos', p2Score = p2Score', p1Turn = p1Turn'}
  where
    rollSum = sum $ take 3 rolls
    rolls' = drop 3 rolls
    p1Turn' = not p1Turn
    p1Pos' = (p1Pos + rollSum) `rem` 10
    p2Pos' = (p2Pos + rollSum) `rem` 10
    p1Score' = p1Score + p1Pos' + 1
    p2Score' = p2Score + p2Pos' + 1
    numRolls' = numRolls + 3

runQuantum :: Game -> Memo Game (Sum Int, Sum Int) (Sum Int, Sum Int)
runQuantum game@Game {..}
  | p1Score >= 21 = return (Sum 1, Sum 0)
  | p2Score >= 21 = return (Sum 0, Sum 1)
  | otherwise = mconcat <$> traverse (memo runQuantum) (roll <$> rolls)
  where
    rolls = [r1 + r2 + r3 | r1 <- [1 .. 3], r2 <- [1 .. 3], r3 <- [1 .. 3]]
    p1Turn' = not p1Turn
    roll n
      | p1Turn = game {p1Pos = p1Pos', p1Score = p1Score', p1Turn = p1Turn'}
      | otherwise = game {p2Pos = p2Pos', p2Score = p2Score', p1Turn = p1Turn'}
      where
        p1Pos' = (p1Pos + n) `rem` 10
        p2Pos' = (p2Pos + n) `rem` 10
        p1Score' = p1Score + p1Pos' + 1
        p2Score' = p2Score + p2Pos' + 1

part1 :: Int
part1 = run (iterate inc100 1) 0 (mkGame 9 3)
  where
    inc100 100 = 1
    inc100 x = x + 1

part2 :: Int
part2 = getSum . uncurry max . startEvalMemo . runQuantum $ mkGame 9 3
