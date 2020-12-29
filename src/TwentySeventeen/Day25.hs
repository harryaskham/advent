module TwentySeventeen.Day25 where

import Data.Char (digitToInt)
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Tuple.Extra (fst3)
import Text.ParserCombinators.Parsec
  ( GenParser,
    char,
    digit,
    eof,
    letter,
    many1,
    sepBy,
    string,
  )
import Util (readWithParser)

inputPath :: String
inputPath = "input/2017/25.txt"

data Branch = Branch Int Int Char deriving (Show)

data Conditional = Conditional Branch Branch deriving (Show)

data Program = Program Int Char (M.Map Char Conditional) deriving (Show)

program :: GenParser Char () Program
program = do
  string "Begin in state "
  startState <- letter
  char '.'
  eol
  string "Perform a diagnostic checksum after "
  steps <- many1 digit
  string " steps."
  eol
  eol
  states <- state `sepBy` eol
  eof
  return $ Program (read steps) startState (M.fromList states)
  where
    eol = char '\n'
    state = do
      string "In state "
      s <- letter
      char ':'
      eol
      b1 <- branch
      b2 <- branch
      return (s, Conditional b1 b2)
    branch = do
      string "  If the current value is "
      digit
      string ":"
      eol
      string "    - Write the value "
      value <- digit
      char '.'
      eol
      string "    - Move one slot to the "
      direction <- many1 letter
      char '.'
      eol
      string "    - Continue with state "
      nextState <- letter
      char '.'
      eol
      return $
        Branch
          (digitToInt value)
          ( case direction of
              "right" -> 1
              "left" -> (-1)
          )
          nextState

step :: M.Map Char Conditional -> (M.Map Int Int, Char, Int) -> (M.Map Int Int, Char, Int)
step conditionals (tape, state, pos) =
  ((M.insert pos value tape), nextState, (pos + offset))
  where
    current = M.findWithDefault 0 pos tape
    (Conditional b0 b1) = conditionals M.! state
    (Branch value offset nextState) = if current == 0 then b0 else b1

part1 :: IO Int
part1 = do
  Program steps startState conditionals <-
    readWithParser program <$> readFile inputPath
  return
    . M.size
    . M.filter (== 1)
    . fst3
    $ foldl'
      (\s -> const $ step conditionals s)
      (M.empty, startState, 0)
      [1 .. steps]
