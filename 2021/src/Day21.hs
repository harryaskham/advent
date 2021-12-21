module Day21 (part1, part2) where

import Data.Array qualified as A
import Data.Bimap (Bimap)
import Data.Bimap qualified as BM
import Data.Fin (Fin)
import Data.Map.Strict qualified as M
import Data.Mod
import Data.PQueue.Prio.Min qualified as PQ
import Data.Sequence qualified as SQ
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Read
import Data.Type.Nat (Mult)
import Data.Vector qualified as V
import Helper.Coord
import Helper.Grid
import Helper.TH
import Helper.Tracers
import Helper.Util
import Text.ParserCombinators.Parsec

data Roll = Classical Int | Quantum (Int, Int, Int)

data Game = Game
  { p1Pos :: Int,
    p2Pos :: Int,
    p1Score :: Int,
    p2Score :: Int,
    p1Turn :: Bool,
    rolls :: [Roll],
    numRolls :: Int
  }

mkGame :: Int -> Int -> [Roll] -> Game
mkGame p1Pos p2Pos rolls = Game (p1Pos - 1) (p2Pos - 1) 0 0 True rolls 0

inc100 :: Roll -> Roll
inc100 (Classical 100) = Classical 1
inc100 (Classical x) = Classical (x + 1)
inc100 _ = error "Can't increment a quantum die"

run :: Game -> Int
run game@Game {..}
  | p1Score >= 1000 = p2Score * numRolls
  | p2Score >= 1000 = p1Score * numRolls
  | p1Turn =
    run $
      game
        { p1Pos = p1Pos',
          p1Score = p1Score',
          p1Turn = p1Turn',
          numRolls = numRolls',
          rolls = rolls'
        }
  | otherwise =
    run $
      game
        { p2Pos = p2Pos',
          p2Score = p2Score',
          p1Turn = p1Turn',
          numRolls = numRolls',
          rolls = rolls'
        }
  where
    [(Classical a), (Classical b), (Classical c)] = take 3 rolls
    rollSum = a + b + c
    rolls' = drop 3 rolls
    p1Turn' = not p1Turn
    p1Pos' = (p1Pos + rollSum) `rem` 10
    p2Pos' = (p2Pos + rollSum) `rem` 10
    p1Score' = p1Score + p1Pos' + 1
    p2Score' = p2Score + p2Pos' + 1
    numRolls' = numRolls + 3

part1 :: Int
part1 = run $ mkGame 9 3 (iterate inc100 (Classical 1))

runQuantum :: Game -> (Int, Int)
runQuantum game@Game {..} = undefined

part2 = uncurry max . runQuantum $ mkGame 9 3 (repeat (Quantum (1, 2, 3)))
