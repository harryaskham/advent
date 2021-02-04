module TwentyFifteen.Day6 where

import Coord (Coord2)
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Grid (Grid)
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

data Light = On | Off | Brightness Integer deriving (Eq, Show)

instance Num Light where
  (Brightness a) + (Brightness b) = Brightness (a + b)
  negate (Brightness a) = Brightness (negate a)
  fromInteger a = Brightness a

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

runOp :: Grid Light -> Operation -> Grid Light
runOp grid (TurnOn ((x1, y1), (x2, y2))) =
  foldl'
    (\g c -> M.insert c On g)
    grid
    [(x, y) | x <- [x1 .. x2], y <- [y1 .. y2]]
runOp grid (TurnOff ((x1, y1), (x2, y2))) =
  foldl'
    (\g c -> M.insert c Off g)
    grid
    [(x, y) | x <- [x1 .. x2], y <- [y1 .. y2]]
runOp grid (Toggle ((x1, y1), (x2, y2))) =
  let toggle On = Off
      toggle Off = On
   in foldl'
        (\g c -> M.adjust toggle c g)
        grid
        [(x, y) | x <- [x1 .. x2], y <- [y1 .. y2]]

part1 :: IO Int
part1 =
  M.size
    . M.filter (== On)
    . foldl'
      runOp
      (M.fromList [((x, y), Off) | x <- [0 .. 999], y <- [0 .. 999]])
    . readWithParser operations
    <$> input 2015 6

runOp2 :: Grid Light -> Operation -> Grid Light
runOp2 grid (TurnOn ((x1, y1), (x2, y2))) =
  foldl'
    (\g c -> M.adjust (+ Brightness 1) c g)
    grid
    [(x, y) | x <- [x1 .. x2], y <- [y1 .. y2]]
runOp2 grid (TurnOff ((x1, y1), (x2, y2))) =
  foldl'
    (\g c -> M.adjust (\(Brightness a) -> if a < 1 then Brightness a else Brightness a - 1) c g)
    grid
    [(x, y) | x <- [x1 .. x2], y <- [y1 .. y2]]
runOp2 grid (Toggle ((x1, y1), (x2, y2))) =
  foldl'
    (\g c -> M.adjust (+ Brightness 2) c g)
    grid
    [(x, y) | x <- [x1 .. x2], y <- [y1 .. y2]]

part2 :: IO Light
part2 =
  sum
    . M.elems
    . foldl'
      runOp2
      (M.fromList [((x, y), Brightness 0) | x <- [0 .. 999], y <- [0 .. 999]])
    . readWithParser operations
    <$> input 2015 6
