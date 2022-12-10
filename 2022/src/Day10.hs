module Day10 (part1, part2) where

import Data.Map.Strict qualified as M
import Helper.Coord (Coord2)
import Helper.Grid (SimpleWall (Wall), fromCoords, pretty)
import Helper.TH (input)
import Helper.Util (eol, number, parseWith)
import Text.ParserCombinators.Parsec (Parser, eof, many1, string, (<|>))
import Prelude hiding ((<|>))

data Instruction = Noop | Addx Int deriving (Show)

data Machine = Machine {register :: Int, cycles :: Int, cycleRegisters :: Map Int Int}

parser :: Parser [Instruction]
parser = many1 (instruction <* eol) <* eof
  where
    instruction = noop <|> addx
    noop = string "noop" >> return Noop
    addx = Addx <$> (string "addx " *> number)

runInstruction :: Machine -> Instruction -> Machine
runInstruction m Noop =
  Machine
    (register m)
    (cycles m + 1)
    (M.insert (cycles m) (register m) (cycleRegisters m))
runInstruction m (Addx x) =
  Machine
    (register m + x)
    (cycles m + 2)
    ((M.insert (cycles m) (register m) . M.insert (cycles m + 1) (register m)) (cycleRegisters m))

run :: [(Int, Int)]
run =
  $(input 10)
    & parseWith parser
    & foldl' runInstruction (Machine 1 1 M.empty)
    & cycleRegisters
    & M.toList

draw :: (Int, Int) -> Maybe Coord2
draw (c, x) =
  let (cx, cy) = ((c - 1) `mod` 40, (c - 1) `div` 40)
   in if cx `elem` [x - 1, x, x + 1] then Just (cx, cy) else Nothing

part1 :: Int
part1 =
  run
    & filter (\(c, _) -> c `elem` [20, 60, 100, 140, 180, 220])
    & fmap (uncurry (*))
    & sum

part2 :: Text
part2 =
  run
    & mapMaybe draw
    & fromCoords Wall
    & pretty
