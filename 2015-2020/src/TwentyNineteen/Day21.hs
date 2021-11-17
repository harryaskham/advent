{-# LANGUAGE QuasiQuotes #-}

module TwentyNineteen.Day21 where

import Control.Lens (view)
import Data.Char (ord)
import Text.RawString.QQ (r)
import TwentyNineteen.Intcode
  ( Machine (Machine),
    outputs,
    readProgram,
    runProgram,
  )

solve :: String -> IO Integer
solve script = do
  program <- readProgram "input/2019/21.txt"
  machine <- runProgram (Machine 0 (fromIntegral . ord <$> script) [] program 0)
  return . last $ view outputs machine

part1 :: IO Integer
part1 =
  solve
    [r|NOT C J
NOT A T
OR T J
NOT D T
NOT T T
AND T J
WALK
|]

part2 :: IO Integer
part2 =
  solve
    [r|NOT A T
OR T J
NOT B T
OR T J
NOT C T
OR T J
AND D J
NOT H T
NOT T T
OR E T
AND T J
RUN
|]
