module TwentyTwenty.Day12 where

import Data.List (foldl')
import Text.ParserCombinators.Parsec
  ( GenParser,
    char,
    digit,
    eof,
    letter,
    many,
    many1,
    parse,
  )

inputPath :: String
inputPath = "input/2020/12.txt"

data Dir = N | S | E | W | L | R | F deriving (Show)

data Action = Action Dir Int deriving (Show)

data Ship = Ship Dir Int Int

dirFromChar :: Char -> Dir
dirFromChar 'N' = N
dirFromChar 'S' = S
dirFromChar 'E' = E
dirFromChar 'W' = W
dirFromChar 'L' = L
dirFromChar 'R' = R
dirFromChar 'F' = F

opposite :: Dir -> Dir
opposite E = W
opposite W = E
opposite N = S
opposite S = N
opposite L = R
opposite R = L

leftOf :: Dir -> Dir
leftOf N = W
leftOf W = S
leftOf S = E
leftOf E = N

rightOf :: Dir -> Dir
rightOf N = E
rightOf E = S
rightOf S = W
rightOf W = N

parseActions :: GenParser Char st [Action]
parseActions = do
  actions <- many line
  eof
  return actions
  where
    line :: GenParser Char st Action
    line = do
      action <- letter
      value <- read <$> many1 digit
      char '\n'
      return $ Action (dirFromChar action) value

turn :: Dir -> Int -> Dir -> Dir
turn _ 180 facing = opposite facing
turn dir 270 facing = turn (opposite dir) 90 facing
turn dir 90 facing = case dir of
  L -> leftOf facing
  R -> rightOf facing

performAction :: Ship -> Action -> Ship
performAction ship@(Ship facing x y) (Action dir val) =
  case dir of
    N -> Ship facing x (y + val)
    S -> Ship facing x (y - val)
    E -> Ship facing (x + val) y
    W -> Ship facing (x - val) y
    L -> Ship (turn L val facing) x y
    R -> Ship (turn R val facing) x y
    F -> performAction ship (Action facing val)

manhattan :: Ship -> Int
manhattan (Ship _ x y) = abs x + abs y

part1 :: IO Int
part1 = do
  Right as <- parse parseActions "[input]" <$> readFile inputPath
  return . manhattan $ foldl' performAction (Ship E 0 0) as

data ShipWaypoint = ShipWaypoint Int Int Int Int

performWaypointAction :: ShipWaypoint -> Action -> ShipWaypoint
performWaypointAction sw@(ShipWaypoint sX sY wX wY) (Action dir val) =
  case dir of
    N -> ShipWaypoint sX sY wX (wY + val)
    S -> ShipWaypoint sX sY wX (wY - val)
    E -> ShipWaypoint sX sY (wX + val) wY
    W -> ShipWaypoint sX sY (wX - val) wY
    L -> rotate L val sw
    R -> rotate R val sw
    F -> ShipWaypoint (sX + val * wX) (sY + val * wY) wX wY

rotate :: Dir -> Int -> ShipWaypoint -> ShipWaypoint
rotate _ 180 (ShipWaypoint sX sY wX wY) =
  ShipWaypoint sX sY (negate wX) (negate wY)
rotate dir 270 sw = rotate (opposite dir) 90 sw
rotate dir 90 (ShipWaypoint sX sY wX wY) =
  case dir of
    L -> ShipWaypoint sX sY (negate wY) wX
    R -> ShipWaypoint sX sY wY (negate wX)

manhattanW :: ShipWaypoint -> Int
manhattanW (ShipWaypoint x y _ _) = abs x + abs y

part2 :: IO Int
part2 = do
  Right as <- parse parseActions "[input]" <$> readFile inputPath
  return . manhattanW $ foldl' performWaypointAction (ShipWaypoint 0 0 10 1) as
