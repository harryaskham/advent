module Day2 (part1, part2) where

import Helper.Util (Solution (..), eol, input, number, parseInput)
import Text.ParserCombinators.Parsec (GenParser, eof, many1, string)

data Movement
  = MForward Integer
  | MDown Integer
  | MUp Integer

parser :: GenParser Char () [Movement]
parser = many1 (line <* eol) <* eof
  where
    line = forward <|> down <|> up
    forward = MForward <$> (string "forward " >> number)
    down = MDown <$> (string "down " >> number)
    up = MUp <$> (string "up " >> number)

data Submarine = Submarine Integer Integer Integer deriving (Show)

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

solve :: (Submarine -> Movement -> Submarine) -> IO Integer
solve moveSubmarine =
  toSolution . foldl' moveSubmarine (Submarine 0 0 0)
    <$> parseInput parser (input 2)

part1 :: IO Integer
part1 = solve moveSubmarine1

part2 :: IO Integer
part2 = solve moveSubmarine2
