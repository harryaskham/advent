module TwentyNineteen.Day5 where

import Control.Lens ((^.))
import TwentyNineteen.Intcode
  ( Machine (Machine),
    outputs,
    readProgram,
    runProgram,
  )

part12 :: IO ([Integer], [Integer])
part12 = do
  program <- readProgram "input/2019/5.txt"
  machine <- runProgram $ Machine 0 [1] [] program 0
  machine' <- runProgram $ Machine 0 [5] [] program 0
  return (machine ^. outputs, machine' ^. outputs)
