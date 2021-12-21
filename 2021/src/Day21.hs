module Day21 (part1, part2) where

import Control.Lens (makeLenses, over, view, (^.))
import Control.Monad.Memo (Memo, MonadMemo (memo), startEvalMemo)

data Game = Game
  { _p1Pos :: Int,
    _p2Pos :: Int,
    _p1Score :: Int,
    _p2Score :: Int,
    _p1Turn :: Bool
  }
  deriving (Eq, Ord)

makeLenses ''Game

mkGame :: Int -> Int -> Game
mkGame p1Pos p2Pos = Game (p1Pos - 1) (p2Pos - 1) 0 0 True

roll :: Game -> Int -> Game
roll game n
  | game ^. p1Turn = go p1Pos p1Pos p1Score
  | otherwise = go p2Pos p2Pos p2Score
  where
    go pPos pPos' pScore =
      game
        & over pPos ((`rem` 10) . (+ n))
        & (\g -> over pScore (+ (view pPos' g + 1)) g)
        & over p1Turn not

run :: [Int] -> Int -> Game -> Int
run rolls numRolls game
  | game ^. p1Score >= 1000 = numRolls * game ^. p2Score
  | game ^. p2Score >= 1000 = numRolls * game ^. p1Score
  | otherwise = run (drop 3 rolls) (numRolls + 3) $ roll game (sum $ take 3 rolls)

runQuantum :: Game -> Memo Game (Sum Int, Sum Int) (Sum Int, Sum Int)
runQuantum game
  | game ^. p1Score >= 21 = return (Sum 1, Sum 0)
  | game ^. p2Score >= 21 = return (Sum 0, Sum 1)
  | otherwise =
    let rolls = [r1 + r2 + r3 | let rs = [1 .. 3], r1 <- rs, r2 <- rs, r3 <- rs]
     in mconcat <$> traverse (memo runQuantum) (roll game <$> rolls)

part1 :: Int
part1 = run (cycle [1 .. 100]) 0 (mkGame 9 3)

part2 :: Int
part2 = getSum . uncurry max . startEvalMemo . runQuantum $ mkGame 9 3
