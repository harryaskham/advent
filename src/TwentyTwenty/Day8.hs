module TwentyTwenty.Day8 where

import Control.Monad.State.Lazy
  ( MonadState (get, put),
    State,
    evalState,
  )
import Data.Maybe (catMaybes)
import qualified Data.Set as S
import qualified Data.Vector as V
import Text.ParserCombinators.Parsec
  ( GenParser,
    char,
    digit,
    eof,
    letter,
    many,
    many1,
    parse,
    spaces,
    (<|>),
  )

inputPath :: String
inputPath = "input/2020/8.txt"

data Instruction = Nop | Acc Int | Jmp Int deriving (Show)

newtype Accumulator = Accumulator Int deriving (Show)

newtype Counter = Counter Int deriving (Show, Ord, Eq)

data Machine = Machine (V.Vector Instruction) Counter Accumulator deriving (Show)

parseInstructions :: GenParser Char st [Instruction]
parseInstructions = do
  instructions <- many line
  eof
  return instructions
  where
    line :: GenParser Char st Instruction
    line = do
      opcode <- many1 letter
      spaces
      sign <- char '+' <|> char '-'
      value <- many1 digit
      char '\n'
      case opcode of
        "nop" -> return Nop
        "acc" -> case sign of
          '+' -> return $ Acc (read value)
          '-' -> return $ Acc (negate $ read value)
        "jmp" -> case sign of
          '+' -> return $ Jmp (read value)
          '-' -> return $ Jmp (negate $ read value)

stepMachine :: Machine -> Machine
stepMachine (Machine is (Counter c) (Accumulator a)) =
  case is V.! c of
    Nop -> Machine is (Counter $ c + 1) (Accumulator a)
    Acc inc -> Machine is (Counter $ c + 1) (Accumulator $ a + inc)
    Jmp inc -> Machine is (Counter $ c + inc) (Accumulator a)

data MachineState = MachineState Machine (S.Set Counter)

accBeforeLooping :: State MachineState Accumulator
accBeforeLooping = do
  (MachineState m@(Machine _ c a) seen) <- get
  if c `S.member` seen
    then return a
    else do
      put $ MachineState (stepMachine m) (S.insert c seen)
      accBeforeLooping

part1 :: IO Accumulator
part1 = do
  f <- readFile inputPath
  case parse parseInstructions "[input]" f of
    Right is -> do
      let machine = Machine (V.fromList is) (Counter 0) (Accumulator 0)
      return $ evalState accBeforeLooping $ MachineState machine S.empty

jmpsToNops :: V.Vector Instruction -> [V.Vector Instruction]
jmpsToNops is = replaceWithNop <$> jmpIxs
  where
    isJmp i = case i of
      Jmp _ -> True
      _ -> False
    jmpIxs = [ix | (ix, i) <- zip [0 ..] (V.toList is), isJmp i]
    replaceWithNop i = is V.// [(i, Nop)]

checkForTermination :: State MachineState (Maybe Accumulator)
checkForTermination = do
  (MachineState m@(Machine is c@(Counter ci) a) seen) <- get
  if ci == length is
    then return (Just a)
    else
      if c `S.member` seen
        then return Nothing
        else do
          put $ MachineState (stepMachine m) (S.insert c seen)
          checkForTermination

part2 :: IO Accumulator
part2 = do
  f <- readFile inputPath
  case parse parseInstructions "[input]" f of
    Right is -> do
      let iss = jmpsToNops (V.fromList is)
          machines = Machine <$> iss <*> pure (Counter 0) <*> pure (Accumulator 0)
          states = MachineState <$> machines <*> pure S.empty
      return $ head $ catMaybes $ evalState checkForTermination <$> states
