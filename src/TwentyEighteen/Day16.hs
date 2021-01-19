module TwentyEighteen.Day16 where

import Coord
import Data.Bits
import Data.Char
import qualified Data.Foldable as F
import Data.Function
import Data.List
import Data.List.Extra
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Sequence (Seq)
import qualified Data.Sequence as SQ
import Data.Set (Set)
import qualified Data.Set as S
import Data.Tuple.Extra
import Data.Vector (Vector)
import qualified Data.Vector as V
import Debug.Trace
import Grid
import Text.ParserCombinators.Parsec
import Util

type Memory = Map Register Int

newtype Register = Register Int deriving (Show, Eq, Ord)

newtype Immediate = Immediate Int deriving (Show, Eq, Ord)

class Evaluable a where
  value :: Memory -> a -> Int

instance Evaluable Register where
  value mem r = mem M.! r

instance Evaluable Immediate where
  value _ (Immediate i) = i

data Instruction
  = AddR Register Register Register
  | AddI Register Immediate Register
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
  deriving (Show)

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
        ds <- digit `sepBy` (string ", ")
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
      [a, b, c, d] <- many1 digit `sepBy` (char ' ')
      eol
      return $ Operation (read a) (read b) (read c) (read d)

runI :: Instruction -> Memory -> Memory
runI (AddR a b c) mem = M.insert c (value mem a + value mem b) mem
runI (AddI a b c) mem = M.insert c (value mem a + value mem b) mem
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

allOps :: [(Int -> Int -> Int -> Instruction)]
allOps =
  [ \a b c -> AddR (Register a) (Register b) (Register c),
    \a b c -> AddI (Register a) (Immediate b) (Register c),
    \a b c -> BandR (Register a) (Register b) (Register c),
    \a b c -> BandI (Register a) (Immediate b) (Register c),
    \a b c -> BorR (Register a) (Register b) (Register c),
    \a b c -> BorI (Register a) (Immediate b) (Register c),
    \a _ c -> SetR (Register a) (Register c),
    \a _ c -> SetI (Immediate a) (Register c),
    \a b c -> GtIR (Immediate a) (Register b) (Register c),
    \a b c -> GtRI (Register a) (Immediate b) (Register c),
    \a b c -> GtRR (Register a) (Register b) (Register c),
    \a b c -> EqIR (Immediate a) (Register b) (Register c),
    \a b c -> EqRI (Register a) (Immediate b) (Register c),
    \a b c -> EqRR (Register a) (Register b) (Register c)
  ]

validOpcodes :: Constraint -> [(Int -> Int -> Int -> Instruction)]
validOpcodes (Constraint before (Operation _ a b c) after) =
  filter (possible a b c) allOps
  where
    possible a b c op = runI (op a b c) before == after

part1 :: IO Int
part1 = do
  (constraints, _) <- readWithParser parseInput <$> input 2018 16
  return . length . filter ((>= 3) . length) $ validOpcodes <$> constraints

part2 :: IO Int
part2 = do
  (constraints, program) <- readWithParser parseInput <$> input 2018 16
  return 0
