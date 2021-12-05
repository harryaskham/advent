module Day2 (part1, part2) where

import Helper.TH (input)
import Helper.Util (Solution (..), number, parseLinesWith)
import Text.ParserCombinators.Parsec (GenParser, choice, string)

data Movement
  = MForward Integer
  | MDown Integer
  | MUp Integer

line :: GenParser Char () Movement
line =
  choice
    [ MForward <$> (string "forward " >> number),
      MDown <$> (string "down " >> number),
      MUp <$> (string "up " >> number)
    ]

data Submarine = Submarine Integer Integer Integer

instance Solution Submarine Integer where
  toSolution (Submarine x h _) = x * h

moveSubmarine1 :: Submarine -> Movement -> Submarine
moveSubmarine1 (Submarine x h a) (MForward v) = Submarine (x + v) h a
moveSubmarine1 (Submarine x h a) (MDown v) = Submarine x (h + v) a
moveSubmarine1 (Submarine x h a) (MUp v) = Submarine x (h - v) a

moveSubmarine2 :: Submarine -> Movement -> Submarine
moveSubmarine2 (Submarine x h a) (MForward v) = Submarine (x + v) (h + a * v) a
moveSubmarine2 (Submarine x h a) (MDown v) = Submarine x h (a + v)
moveSubmarine2 (Submarine x h a) (MUp v) = Submarine x h (a - v)

solve :: (Submarine -> Movement -> Submarine) -> Integer
solve moveSubmarine =
  $(input 2)
    & parseLinesWith line
    & foldl' moveSubmarine (Submarine 0 0 0)
    & toSolution

part1 :: Integer
part1 = solve moveSubmarine1

part2 :: Integer
part2 = solve moveSubmarine2
