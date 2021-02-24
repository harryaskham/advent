module TwentyNineteen.Day9 where

import Control.Lens ((^.))
import TwentyNineteen.Intcode
  ( Machine (Machine),
    outputs,
    readProgram,
    runProgram,
  )

part12 :: IO ([Integer], [Integer])
part12 = do
  program <- readProgram "input/2019/9.txt"
  machine1 <- runProgram $ Machine 0 [1] [] program 0
  machine2 <- runProgram $ Machine 0 [2] [] program 0
  return $ (machine1 ^. outputs, machine2 ^. outputs)
