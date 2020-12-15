{-# LANGUAGE Strict #-}

module TwentyTwenty.Day15 where

import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M

input :: [Int]
input = read <$> splitOn "," "20,0,1,11,6,3"

data GameState = GameState Int Int (M.Map Int [Int]) deriving (Show)

mkGameState :: [Int] -> GameState
mkGameState xs =
  GameState
    (length xs + 1)
    (last xs)
    (M.fromList (zip xs (pure <$> [1 ..])))

stepGame :: GameState -> GameState
stepGame (GameState step lastSaid ages) =
  let nextSaid =
        case M.lookup lastSaid ages of
          Just [_] -> 0
          Just [age1, age2] -> age1 - age2
      nextAges =
        case M.lookup nextSaid ages of
          Nothing -> M.insert nextSaid [step] ages
          Just (age : _) -> M.insert nextSaid [step, age] ages
   in GameState (step + 1) nextSaid nextAges

stepUntil :: Int -> GameState -> GameState
stepUntil target state@(GameState step _ _) =
  if step > target
    then state
    else stepUntil target (stepGame state)

part1 :: Int
part1 = let (GameState _ x _) = stepUntil 2020 (mkGameState input) in x

part2 :: Int
part2 = let (GameState _ x _) = stepUntil 30000000 (mkGameState input) in x
