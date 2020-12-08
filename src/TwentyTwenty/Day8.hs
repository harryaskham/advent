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

data Instruction = Nop Int | Acc Int | Jmp Int deriving (Show)

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
        "nop" -> case sign of
          '+' -> return $ Nop (read value)
          '-' -> return $ Nop (negate $ read value)
        "acc" -> case sign of
          '+' -> return $ Acc (read value)
          '-' -> return $ Acc (negate $ read value)
        "jmp" -> case sign of
          '+' -> return $ Jmp (read value)
          '-' -> return $ Jmp (negate $ read value)

stepMachine :: Machine -> Machine
stepMachine (Machine is (Counter c) (Accumulator a)) =
  case is V.! c of
    Nop _ -> Machine is (Counter $ c + 1) (Accumulator a)
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

readInstructions :: IO [Instruction]
readInstructions = do
  f <- readFile inputPath
  case parse parseInstructions "[input]" f of
    Right is -> return is

part1 :: IO Accumulator
part1 = do
  is <- readInstructions
  let machine = Machine (V.fromList is) (Counter 0) (Accumulator 0)
  return $ evalState accBeforeLooping $ MachineState machine S.empty

jmpNopSwaps :: V.Vector Instruction -> [V.Vector Instruction]
jmpNopSwaps is = (replaceWithNop <$> jmpIxs) ++ (replaceWithJmp <$> nopIxs)
  where
    isJmp i = case i of
      Jmp _ -> True
      _ -> False
    isNop i = case i of
      Nop _ -> True
      _ -> False
    jmpIxs = [ix | (ix, i) <- zip [0 ..] (V.toList is), isJmp i]
    nopIxs = [ix | (ix, i) <- zip [0 ..] (V.toList is), isNop i]
    replaceWithNop ix = let (Jmp v) = is V.! ix in is V.// [(ix, Nop v)]
    replaceWithJmp ix = let (Nop v) = is V.! ix in is V.// [(ix, Jmp v)]

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
  is <- readInstructions
  let iss = jmpNopSwaps (V.fromList is)
      machines = Machine <$> iss <*> pure (Counter 0) <*> pure (Accumulator 0)
      states = MachineState <$> machines <*> pure S.empty
  return $ head $ catMaybes $ evalState checkForTermination <$> states
