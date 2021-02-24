module TwentyNineteen.Day19 where

import Control.Lens (view)
import qualified Data.Map.Strict as M
import qualified Data.Matrix as MX
import TwentyNineteen.Intcode
  ( Machine (Machine),
    outputs,
    readProgram,
    runProgram,
  )
import Util (unjust)

part1 :: IO ()
part1 = do
  program <- readProgram "input/2019/19.txt"
  let inputs = [[x, y] | x <- [0 .. 49], y <- [0 .. 49]]
      machines = [Machine 0 i [] program 0 | i <- inputs]
  machines' <- sequenceA $ runProgram <$> machines
  print $ sum $ concat (view outputs <$> machines')

part2 :: IO ()
part2 = do
  program <- readProgram "input/2019/19.txt"
  let inputs = [[x, y] | x <- [0 .. 49], y <- [0 .. 49]]
      machines = M.fromList [(i, Machine 0 i [] program 0) | i <- inputs]
  runMachines <- sequenceA $ runProgram <$> machines
  let coords = view outputs <$> runMachines
  let grid = MX.matrix 50 50 (\(y, x) -> head . unjust $ M.lookup [fromIntegral x -1, fromIntegral y -1] coords)
  print $ MX.prettyMatrix grid
