module TwentySeventeen.Day23 where

import qualified Data.Map.Strict as M
import Data.Numbers.Primes
import TwentySeventeen.Day18
  ( Instruction (Jnz, Mul, Set, Sub),
    Value (Register),
    coerce,
    instructions,
  )
import Util (readWithParser)

inputPath :: String
inputPath = "input/2017/23.txt"

run :: M.Map Char Int -> M.Map Int Instruction -> Int -> Int -> Int
run mem is pc numMuls =
  case M.lookup pc is of
    Nothing -> numMuls
    Just (Set (Register r) v) -> run (M.insert r (coerce mem v) mem) is (pc + 1) numMuls
    Just (Sub r@(Register r') v) -> run (M.insert r' (coerce mem r - coerce mem v) mem) is (pc + 1) numMuls
    Just (Mul r@(Register r') v) -> run (M.insert r' (coerce mem r * coerce mem v) mem) is (pc + 1) (numMuls + 1)
    Just (Jnz v1 v2) ->
      if coerce mem v1 /= 0
        then run mem is (pc + coerce mem v2) numMuls
        else run mem is (pc + 1) numMuls

part1 :: IO Int
part1 = do
  is <- readWithParser instructions <$> readFile inputPath
  return $ run M.empty (M.fromList $ zip [0 ..] is) 0 0

part2 :: Int
part2 =
  length
    [ x
      | x <- [106500, 106500 + 17 .. 106500 + 17000],
        not (isPrime x)
    ]
