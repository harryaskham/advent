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
      ("banr", mkBandr),
      ("bani", mkBandi),
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
      uncurry3 (mkOps M.! name) <$> abc

data Machine = Machine Register Int (Map Int Instruction) Memory deriving (Show, Eq)

stepMachine :: Machine -> Maybe Machine
stepMachine (Machine boundReg ip is mem)
  | not (ip `M.member` is) = Nothing
  | otherwise = Just $ Machine boundReg ip' is mem'
  where
    i = is M.! ip
    mem' = runI i . M.insert boundReg ip $ mem
    ip' = mem' M.! boundReg + 1

runMachine :: Machine -> Memory
runMachine m@(Machine _ _ _ mem) =
  case stepMachine m of
    Nothing -> mem
    Just m -> runMachine m

part1 :: IO Int
part1 = do
  (boundReg, is) <- readWithParser instructions <$> input 2018 19
  let mem = M.fromList (zip (Register <$> [0 .. 5]) (repeat 0))
      machine = Machine (Register boundReg) 0 is mem
      mem' = runMachine machine
  return $ mem' M.! Register 0

part2 :: Int
part2 = sum [a | a <- [1 .. 10551288], 10551288 `mod` a == 0]
