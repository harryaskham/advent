module TwentyNineteen.Day11 where

import Control.Lens ((%~), (.~), (^.))
import Data.Foldable (traverse_)
import Data.Function ((&))
import qualified Data.Set as S
import TwentyNineteen.Intcode
  ( Machine (Machine),
    inputs,
    isTerminated,
    outputs,
    readProgram,
    stepUntilNOutputs,
  )

data Direction
  = Up'
  | Down'
  | Left'
  | Right'
  deriving (Show)

type Position = (Int, Int)

-- A robot has a direction, its brain, its current position, and its set of whites, its set of visited
data Robot = Robot Direction Machine Position (S.Set Position) (S.Set Position) deriving (Show)

stepRobot :: Robot -> IO Robot
stepRobot (Robot direction machine position@(x, y) whites seen) = do
  let isWhite = position `S.member` whites
      currentMachine = machine & inputs %~ if isWhite then (1 :) else (0 :)
  nextMachine <- stepUntilNOutputs 2 currentMachine
  let [colorI, rotI] = nextMachine ^. outputs
      nextWhites = case colorI of
        0 -> S.delete position whites
        1 -> S.insert position whites
      nextSeen = S.insert position seen
      nextDirection = case rotI of
        0 -> case direction of
          Up' -> Left'
          Left' -> Down'
          Down' -> Right'
          Right' -> Up'
        1 -> case direction of
          Up' -> Right'
          Right' -> Down'
          Down' -> Left'
          Left' -> Up'
      nextPosition = case nextDirection of
        Up' -> (x, y -1)
        Down' -> (x, y + 1)
        Left' -> (x -1, y)
        Right' -> (x + 1, y)
  if isTerminated nextMachine
    then return (Robot direction nextMachine position whites seen)
    else return $ Robot nextDirection (nextMachine & outputs .~ []) nextPosition nextWhites nextSeen

stepRobotForever :: Robot -> IO Robot
stepRobotForever robot@(Robot _ machine _ _ _) =
  if isTerminated machine
    then return robot
    else stepRobotForever =<< stepRobot robot

part1 :: IO Int
part1 = do
  program <- readProgram "input/2019/11.txt"
  let robot = Robot Up' (Machine 0 [] [] program 0) (0, 0) S.empty S.empty
  (Robot _ _ _ _ seen) <- stepRobotForever robot
  return $ length seen

part2 :: IO ()
part2 = do
  program <- readProgram "input/2019/11.txt"
  let robot = Robot Up' (Machine 0 [] [] program 0) (0, 0) (S.singleton (0, 0)) S.empty
  (Robot _ _ _ whites _) <- stepRobotForever robot
  traverse_
    print
    [ [ if (x, y) `S.member` whites then 'X' else ' '
        | x <- [0 .. (maximum $ S.map fst whites)]
      ]
      | y <- [0 .. (maximum $ S.map snd whites)]
    ]
