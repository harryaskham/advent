{-# LANGUAGE RecordWildCards #-}

module TwentyEighteen.Day9 where

import Control.Monad (when)
import qualified Data.Sequence as SQ
import qualified Data.Vector as V

data Game = Game
  { marbles :: SQ.Seq Int,
    scores :: V.Vector Int,
    currentMarble :: Int,
    nextCount :: Int,
    nextPlayer :: Int,
    numPlayers :: Int
  }
  deriving (Show)

-- Creates a new game with the initial marble placed.
newGame :: Int -> Game
newGame numPlayers =
  Game
    { marbles = SQ.fromList [0],
      scores = V.replicate numPlayers 0,
      currentMarble = 0,
      nextCount = 1,
      nextPlayer = 0,
      numPlayers = numPlayers
    }

-- Run a single turn and return what the last marble was worth.
runTurn :: Game -> (Game, Int)
runTurn game =
  if (nextCount game `mod` 23) == 0
    then run23Turn game
    else runRegularTurn game

run23Turn :: Game -> (Game, Int)
run23Turn Game {..} =
  ( Game
      { marbles = marbles',
        scores = scores V.// [(nextPlayer, (scores V.! nextPlayer) + score)],
        currentMarble = (removeIndex + 2) `mod` length marbles',
        nextCount = nextCount + 1,
        nextPlayer = (nextPlayer + 1) `mod` numPlayers,
        numPlayers = numPlayers
      },
    score
  )
  where
    removeIndex = (currentMarble - 9 + length marbles) `mod` length marbles
    score = nextCount + marbles `SQ.index` removeIndex
    marbles' = SQ.deleteAt removeIndex marbles

runRegularTurn :: Game -> (Game, Int)
runRegularTurn Game {..} =
  ( Game
      { marbles = marbles',
        scores = scores,
        currentMarble = (currentMarble + 2) `mod` length marbles',
        nextCount = nextCount + 1,
        nextPlayer = (nextPlayer + 1) `mod` numPlayers,
        numPlayers = numPlayers
      },
    0
  )
  where
    marbles' = SQ.insertAt currentMarble nextCount marbles

-- Runs the game until the last marble score matches the target.
runTurnUntilPoints :: Int -> Game -> IO Game
runTurnUntilPoints pointsTarget game = do
  when (nextCount game `mod` 1000 == 0) (print $ nextCount game)
  if nextCount game == (pointsTarget + 1)
    then return game
    else runTurnUntilPoints pointsTarget nextGame
  where
    (nextGame, points) = runTurn game

part1 :: IO Int
part1 = do
  game <- runTurnUntilPoints 72058 (newGame 426)
  return $ maximum . scores $ game

part2 :: IO Int
part2 = do
  game <- runTurnUntilPoints 7205800 (newGame 426)
  return $ maximum . scores $ game
