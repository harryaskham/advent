{-# LANGUAGE DeriveDataTypeable #-}

module TwentyEighteen.Day16 where

import Data.Bits (Bits ((.&.), (.|.)))
import Data.Char (digitToInt)
import Data.Data (Data (toConstr), showConstr)
import Data.List (foldl', (\\))
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as S
import Text.ParserCombinators.Parsec
  ( GenParser,
    char,
    digit,
    eof,
    many,
    many1,
    sepBy,
    spaces,
    string,
  )
import Util (adjustWithDefault, eol, input, readWithParser)

type Memory = Map Register Int

newtype Register = Register Int deriving (Show, Eq, Ord, Data)

newtype Immediate = Immediate Int deriving (Show, Eq, Ord, Data)

class Evaluable a where
  value :: Memory -> a -> Int

instance Evaluable Register where
  value mem r = mem M.! r

instance Evaluable Immediate where
  value _ (Immediate i) = i

data Instruction
  = AddR Register Register Register
  | AddI Register Immediate Register
  | MulR Register Register Register
  | MulI Register Immediate Register
  | BandR Register Register Register
  | BandI Register Immediate Register
  | BorR Register Register Register
  | BorI Register Immediate Register
  | SetR Register Register
  | SetI Immediate Register
  | GtIR Immediate Register Register
  | GtRI Register Immediate Register
  | GtRR Register Register Register
  | EqIR Immediate Register Register
  | EqRI Register Immediate Register
  | EqRR Register Register Register
  deriving (Show, Data)

instance Ord Instruction where
  compare a b = compare (showConstr $ toConstr a) (showConstr $ toConstr b)

instance Eq Instruction where
  a == b = toConstr a == toConstr b

data Operation = Operation Int Int Int Int deriving (Show)

data Constraint = Constraint Memory Operation Memory deriving (Show)

type Program = [Operation]

parseInput :: GenParser Char () ([Constraint], Program)
parseInput = do
  cs <- many constraint
  eol >> eol
  ops <- many operation
  eof
  return (cs, ops)
  where
    memory :: String -> GenParser Char () Memory
    memory s = do
      rs <- do
        string s
        char ':'
        spaces
        char '['
        ds <- digit `sepBy` string ", "
        char ']'
        eol
        return $ digitToInt <$> ds
      return $ M.fromList (zip (Register <$> [0 ..]) rs)
    constraint :: GenParser Char () Constraint
    constraint = do
      before <- memory "Before"
      op <- operation
      after <- memory "After"
      eol
      return $ Constraint before op after
    operation :: GenParser Char () Operation
    operation = do
      [a, b, c, d] <- many1 digit `sepBy` char ' '
      eol
      return $ Operation (read a) (read b) (read c) (read d)

runI :: Instruction -> Memory -> Memory
runI (AddR a b c) mem = M.insert c (value mem a + value mem b) mem
runI (AddI a b c) mem = M.insert c (value mem a + value mem b) mem
runI (MulR a b c) mem = M.insert c (value mem a * value mem b) mem
runI (MulI a b c) mem = M.insert c (value mem a * value mem b) mem
runI (BandR a b c) mem = M.insert c (value mem a .&. value mem b) mem
runI (BandI a b c) mem = M.insert c (value mem a .&. value mem b) mem
runI (BorR a b c) mem = M.insert c (value mem a .|. value mem b) mem
runI (BorI a b c) mem = M.insert c (value mem a .|. value mem b) mem
runI (SetR a c) mem = M.insert c (value mem a) mem
runI (SetI a c) mem = M.insert c (value mem a) mem
runI (GtIR a b c) mem = M.insert c (if value mem a > value mem b then 1 else 0) mem
runI (GtRI a b c) mem = M.insert c (if value mem a > value mem b then 1 else 0) mem
runI (GtRR a b c) mem = M.insert c (if value mem a > value mem b then 1 else 0) mem
runI (EqIR a b c) mem = M.insert c (if value mem a == value mem b then 1 else 0) mem
runI (EqRI a b c) mem = M.insert c (if value mem a == value mem b then 1 else 0) mem
runI (EqRR a b c) mem = M.insert c (if value mem a == value mem b then 1 else 0) mem

mkAddr a b c = AddR (Register a) (Register b) (Register c)

mkAddi a b c = AddI (Register a) (Immediate b) (Register c)

mkMulr a b c = MulR (Register a) (Register b) (Register c)

mkMuli a b c = MulI (Register a) (Immediate b) (Register c)

mkBandr a b c = BandR (Register a) (Register b) (Register c)

mkBandi a b c = BandI (Register a) (Immediate b) (Register c)

mkBorr a b c = BorR (Register a) (Register b) (Register c)

mkBori a b c = BorI (Register a) (Immediate b) (Register c)

mkSetr a _ c = SetR (Register a) (Register c)

mkSeti a _ c = SetI (Immediate a) (Register c)

mkGtir a b c = GtIR (Immediate a) (Register b) (Register c)

mkGtri a b c = GtRI (Register a) (Immediate b) (Register c)

mkGtrr a b c = GtRR (Register a) (Register b) (Register c)

mkEqir a b c = EqIR (Immediate a) (Register b) (Register c)

mkEqri a b c = EqRI (Register a) (Immediate b) (Register c)

mkEqrr a b c = EqRR (Register a) (Register b) (Register c)

allOps :: [Int -> Int -> Int -> Instruction]
allOps =
  [ mkAddr,
    mkAddi,
    mkMulr,
    mkMuli,
    mkBandr,
    mkBandi,
    mkBorr,
    mkBori,
    mkSetr,
    mkSeti,
    mkGtir,
    mkGtri,
    mkGtrr,
    mkEqir,
    mkEqri,
    mkEqrr
  ]

validOpcodes :: Constraint -> [Int -> Int -> Int -> Instruction]
validOpcodes (Constraint before (Operation _ a b c) after) =
  filter (possible a b c) allOps
  where
    possible a b c op = runI (op a b c) before == after

part1 :: IO Int
part1 = do
  (constraints, _) <- readWithParser parseInput <$> input 2018 16
  return . length . filter ((>= 3) . length) $ validOpcodes <$> constraints

inferOps :: [Constraint] -> Map Int (Set Instruction)
inferOps =
  foldl'
    ( \opMap con@(Constraint _ (Operation opcode a b c) _) ->
        adjustWithDefault
          (S.fromList (allOps <*> [a] <*> [b] <*> [c]))
          (S.intersection $ S.fromList (validOpcodes con <*> [a] <*> [b] <*> [c]))
          opcode
          opMap
    )
    M.empty

findAssignment :: Map Int (Set Instruction) -> Maybe (Map Int Instruction)
findAssignment opSetMap = go S.empty M.empty
  where
    go :: Set Instruction -> Map Int Instruction -> Maybe (Map Int Instruction)
    go assignedOpcodes opMap
      | null unassignedOpcodes = Just opMap
      | otherwise =
        case nextRuns of
          [] -> Nothing
          next -> case catMaybes next of
            [] -> Nothing
            (m : _) -> Just m
      where
        unassignedOpcodes = M.keys opSetMap \\ M.keys opMap
        opcode = head unassignedOpcodes
        possibleOps = S.difference (opSetMap M.! opcode) assignedOpcodes
        nextRuns =
          fmap
            (\op -> go (S.insert op assignedOpcodes) (M.insert opcode op opMap))
            (S.toList possibleOps)

runInstruction :: Map Int Instruction -> Operation -> Memory -> Memory
runInstruction opMap (Operation opcode a b c) mem =
  flip runI mem
    . head
    . filter (== (opMap M.! opcode))
    $ (allOps <*> [a] <*> [b] <*> [c])

part2 :: IO Int
part2 = do
  (constraints, program) <- readWithParser parseInput <$> input 2018 16
  let Just opMap = findAssignment $ inferOps constraints
      mem =
        foldl'
          (flip $ runInstruction opMap)
          (M.fromList $ zip (Register <$> [0 ..]) (replicate 4 0))
          program
  return $ mem M.! Register 0
