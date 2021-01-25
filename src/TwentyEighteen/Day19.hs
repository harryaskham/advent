module TwentyEighteen.Day19 where

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
import TwentyEighteen.Day16
import Util

mkOps :: Map String (Int -> Int -> Int -> Instruction)
mkOps =
  M.fromList
    [ ("addr", mkAddr),
      ("addi", mkAddi),
      ("mulr", mkMulr),
      ("muli", mkMuli),
      ("bandr", mkBandr),
      ("bandi", mkBandi),
      ("borr", mkBorr),
      ("bori", mkBori),
      ("setr", mkSetr),
      ("seti", mkSeti),
      ("gtir", mkGtir),
      ("gtri", mkGtri),
      ("gtrr", mkGtrr),
      ("eqir", mkEqir),
      ("eqri", mkEqri),
      ("eqrr", mkEqrr)
    ]

instructions :: GenParser Char () (Int, Map Int Instruction)
instructions = do
  string "#ip "
  ip <- digitToInt <$> digit
  eol
  is <- many1 instruction
  eof
  return (ip, M.fromList (zip [0 ..] is))
  where
    instruction = do
      i <- choice (try <$> (op <$> M.keys mkOps))
      eol
      return i
    abc = do
      a <- read <$> many1 digit
      char ' '
      b <- read <$> many1 digit
      char ' '
      c <- read <$> many1 digit
      return (a, b, c)
    op name = do
      string name
      char ' '
      args <- abc
      return $ uncurry3 (mkOps M.! name) args

data Machine = Machine Register Int (Map Int Instruction) Memory deriving (Show)

runMachine :: Machine -> Memory
runMachine (Machine boundReg ip is mem)
  | not (ip `M.member` is) = mem
  | otherwise = runMachine $ Machine boundReg ip' is mem'
  where
    i = is M.! ip
    mem' = runI i . M.insert boundReg ip $ mem
    ip' = mem' M.! boundReg + 1

part1 :: IO Int
part1 = do
  (boundReg, is) <- readWithParser instructions <$> input 2018 19
  let mem = M.fromList (zip (Register <$> [0 .. 5]) (repeat 0))
      machine = Machine (Register boundReg) 0 is mem
      mem' = runMachine machine
  return $ mem' M.! Register 0

-- TODO: Will need to manually figure out what the program's doing

part2 :: IO Int
part2 = do
  (boundReg, is) <- readWithParser instructions <$> input 2018 19
  let mem = M.fromList (zip (Register <$> [0 .. 5]) (repeat 0))
      machine = Machine (Register boundReg) 0 is (M.insert (Register 0) 1 mem)
      mem' = runMachine machine
  return $ mem' M.! Register 0
