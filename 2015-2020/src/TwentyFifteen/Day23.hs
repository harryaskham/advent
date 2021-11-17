module TwentyFifteen.Day23 where

import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.ParserCombinators.Parsec
  ( GenParser,
    char,
    choice,
    eof,
    many,
    oneOf,
    optional,
    string,
    try,
  )
import Util (eol, input, number, readWithParser)

type Memory = Map Char Int

type Instruction = (Memory, Int) -> (Memory, Int)

instructions :: GenParser Char () [Instruction]
instructions = many instruction <* eof
  where
    instruction = (choice $ try <$> [hlf, tpl, inc, jmp, jie, jio]) <* eol
    regAp (f, g) r (mem, pc) = (M.adjust f r mem, g pc)
    jmpAp f r off (mem, pc) = if f (mem M.! r) then (mem, pc + off) else (mem, pc + 1)
    reg = oneOf "ab"
    offset = (optional (char '+')) *> number
    hlf = regAp ((`div` 2), (+ 1)) <$> (string "hlf " *> reg)
    tpl = regAp ((* 3), (+ 1)) <$> (string "tpl " *> reg)
    inc = regAp ((+ 1), (+ 1)) <$> (string "inc " *> reg)
    jmp = (\off -> regAp (id, (+ off)) '_') <$> (string "jmp " >> offset)
    jie = jmpAp even <$> (string "jie " *> reg) <* string ", " <*> offset
    jio = jmpAp (== 1) <$> (string "jio " *> reg) <* string ", " <*> offset

run :: Vector Instruction -> (Memory, Int) -> Int
run is (mem, pc)
  | pc < 0 || pc >= length is = mem M.! 'b'
  | otherwise =
    run is ((is V.! pc) (mem, pc))

part12 :: IO (Int, Int)
part12 = do
  is <- V.fromList . readWithParser instructions <$> input 2015 23
  return $
    ( run is (M.fromList [('a', 0), ('b', 0)], 0),
      run is (M.fromList [('a', 1), ('b', 0)], 0)
    )
