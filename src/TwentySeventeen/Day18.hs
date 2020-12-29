module TwentySeventeen.Day18 where

import qualified Data.Map.Strict as M
import Data.Maybe
import Text.ParserCombinators.Parsec
import Util

inputPath :: String
inputPath = "input/2017/18.txt"

data Value = Register Char | Literal Int deriving (Eq, Ord, Show)

newtype Frequency = Frequency Int

data Instruction
  = Snd Value
  | Set Value Value
  | Add Value Value
  | Mul Value Value
  | Mod Value Value
  | Rcv Value
  | Jgz Value Value
  | Sub Value Value
  | Jnz Value Value
  deriving (Show)

instructions :: GenParser Char () [Instruction]
instructions = do
  is <- many1 instruction
  eof
  return is
  where
    eol = char '\n'
    instruction = do
      op <- count 3 letter
      char ' '
      v1 <- value
      case op of
        "snd" -> eol >> return (Snd v1)
        "rcv" -> eol >> return (Rcv v1)
        _ -> do
          char ' '
          v2 <- value
          eol
          case op of
            "set" -> return $ Set v1 v2
            "add" -> return $ Add v1 v2
            "mul" -> return $ Mul v1 v2
            "mod" -> return $ Mod v1 v2
            "jgz" -> return $ Jgz v1 v2
            "sub" -> return $ Sub v1 v2
            "jnz" -> return $ Jnz v1 v2
    value =
      choice
        [ try (Register <$> letter),
          try (Literal . read <$> many1 (oneOf "-0123456789"))
        ]

coerce :: M.Map Char Int -> Value -> Int
coerce _ (Literal v) = v
coerce mem (Register r) = fromMaybe 0 $ M.lookup r mem

run :: M.Map Char Int -> Maybe Int -> M.Map Int Instruction -> Int -> Maybe Int
run mem snd is pc =
  case M.lookup pc is of
    Nothing -> Nothing
    Just (Set (Register r) v) -> run (M.insert r (coerce mem v) mem) snd is (pc + 1)
    Just (Add r@(Register r') v) -> run (M.insert r' (coerce mem r + coerce mem v) mem) snd is (pc + 1)
    Just (Mul r@(Register r') v) -> run (M.insert r' (coerce mem r * coerce mem v) mem) snd is (pc + 1)
    Just (Mod r@(Register r') v) -> run (M.insert r' (coerce mem r `mod` coerce mem v) mem) snd is (pc + 1)
    Just (Snd v) -> run mem (Just $ coerce mem v) is (pc + 1)
    Just (Rcv v) -> if coerce mem v > 0 then snd else run mem snd is (pc + 1)
    Just (Jgz v1 v2) ->
      if coerce mem v1 > 0
        then run mem snd is (pc + coerce mem v2)
        else run mem snd is (pc + 1)

part1 :: IO (Maybe Int)
part1 = do
  is <- readWithParser instructions <$> readFile inputPath
  return $ run M.empty Nothing (M.fromList $ zip [0 ..] is) 0

type Machine = (M.Map Char Int, Int, [Int], Int)

data MachineState = Running (Machine, Machine) | Blocked (Machine, Machine) | Terminated (Machine, Machine)

runPair :: M.Map Int Instruction -> Machine -> Machine -> MachineState
runPair is machine@(mem, pc, queue, sends) other@(otherMem, otherPc, otherQueue, otherSends) =
  case M.lookup pc is of
    Nothing -> Terminated (machine, other)
    Just (Set (Register r) v) -> Running ((M.insert r (coerce mem v) mem, pc + 1, queue, sends), other)
    Just (Add r@(Register r') v) -> Running ((M.insert r' (coerce mem r + coerce mem v) mem, pc + 1, queue, sends), other)
    Just (Mul r@(Register r') v) -> Running ((M.insert r' (coerce mem r * coerce mem v) mem, pc + 1, queue, sends), other)
    Just (Mod r@(Register r') v) -> Running ((M.insert r' (coerce mem r `mod` coerce mem v) mem, pc + 1, queue, sends), other)
    Just (Snd v) -> Running ((mem, pc + 1, queue, sends + 1), (otherMem, otherPc, otherQueue ++ [coerce mem v], otherSends))
    Just (Rcv (Register r)) ->
      case queue of
        [] -> Blocked (machine, other)
        (x : queue') -> Running ((M.insert r x mem, pc + 1, queue', sends), other)
    Just (Jgz v1 v2) ->
      if coerce mem v1 > 0
        then Running ((mem, pc + coerce mem v2, queue, sends), other)
        else Running ((mem, pc + 1, queue, sends), other)

runUntil :: M.Map Int Instruction -> Bool -> Machine -> Machine -> Int
runUntil is isOne machine1 machine2 =
  case runPair is machine1 machine2 of
    Running (machine1', machine2') -> runUntil is isOne machine1' machine2'
    Blocked _ -> case next of
      Blocked ((_, _, _, s1), (_, _, _, s2)) -> if isOne then s2 else s1
      _ -> runUntil is (not isOne) machine2 machine1
    Terminated _ -> case next of
      Terminated ((_, _, _, s1), (_, _, _, s2)) -> if isOne then s2 else s1
      _ -> runUntil is (not isOne) machine2 machine1
  where
    next = runPair is machine2 machine1

part2 :: IO Int
part2 = do
  is <- readWithParser instructions <$> readFile inputPath
  let m0 = (M.singleton 'p' 0, 0, [], 0)
      m1 = (M.singleton 'p' 1, 0, [], 0)
  return $ runUntil (M.fromList $ zip [0 ..] is) False m0 m1
