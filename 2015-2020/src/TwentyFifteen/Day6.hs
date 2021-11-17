module TwentyFifteen.Day6 where

import Coord (Coord2)
import qualified Data.Array.Unboxed as A
import Data.List (foldl')
import Grid (AGrid)
import Text.ParserCombinators.Parsec
  ( GenParser,
    char,
    eof,
    many,
    sepBy,
    string,
    try,
    (<|>),
  )
import Util (eol, input, number, readWithParser, toTuple2)

data Operation
  = TurnOn (Coord2, Coord2)
  | TurnOff (Coord2, Coord2)
  | Toggle (Coord2, Coord2)

operations :: GenParser Char () [Operation]
operations = many operation <* eof
  where
    operation = (on <|> off <|> toggle) <* eol
    coord2 = toTuple2 <$> number `sepBy` char ','
    coords = do
      c1 <- coord2
      string " through "
      c2 <- coord2
      return (c1, c2)
    on = try $ TurnOn <$> (string "turn on " *> coords)
    off = try $ TurnOff <$> (string "turn off " *> coords)
    toggle = try $ Toggle <$> (string "toggle " *> coords)

runOp :: AGrid Bool -> Operation -> AGrid Bool
runOp grid (TurnOn ((x1, y1), (x2, y2))) =
  grid A.// [((x, y), True) | x <- [x1 .. x2], y <- [y1 .. y2]]
runOp grid (TurnOff ((x1, y1), (x2, y2))) =
  grid A.// [((x, y), False) | x <- [x1 .. x2], y <- [y1 .. y2]]
runOp grid (Toggle ((x1, y1), (x2, y2))) =
  grid A.// [((x, y), not $ grid A.! (x, y)) | x <- [x1 .. x2], y <- [y1 .. y2]]

part1 :: IO Int
part1 =
  length
    . filter (id)
    . A.elems
    . foldl'
      runOp
      (A.array ((0, 0), (999, 999)) [((x, y), False) | x <- [0 .. 999], y <- [0 .. 999]])
    . readWithParser operations
    <$> input 2015 6

runOp2 :: AGrid Int -> Operation -> AGrid Int
runOp2 grid (TurnOn ((x1, y1), (x2, y2))) =
  A.accum (+) grid [((x, y), 1) | x <- [x1 .. x2], y <- [y1 .. y2]]
runOp2 grid (TurnOff ((x1, y1), (x2, y2))) =
  A.accum (\i a -> if i == 0 then 0 else i - a) grid [((x, y), 1) | x <- [x1 .. x2], y <- [y1 .. y2]]
runOp2 grid (Toggle ((x1, y1), (x2, y2))) =
  A.accum (+) grid [((x, y), 2) | x <- [x1 .. x2], y <- [y1 .. y2]]

part2 :: IO Int
part2 =
  sum
    . A.elems
    . foldl'
      runOp2
      (A.array ((0, 0), (999, 999)) [((x, y), 0) | x <- [0 .. 999], y <- [0 .. 999]])
    . readWithParser operations
    <$> input 2015 6
