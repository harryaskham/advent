module TwentyNineteen.Day19 where

import Control.Lens (view)
import Coord (Coord2, manhattan0)
import Data.List.Extra (minimumOn)
import System.IO.Unsafe (unsafePerformIO)
import TwentyNineteen.Intcode
  ( Machine (Machine),
    Program,
    outputs,
    readProgram,
    runProgram,
  )

part1 :: IO Integer
part1 = do
  program <- readProgram "input/2019/19.txt"
  let inputs = [[x, y] | x <- [0 .. 49], y <- [0 .. 49]]
  machines <- traverse runProgram [Machine 0 i [] program 0 | i <- inputs]
  return . sum . concat $ traverse (view outputs) machines

getCoord :: Program -> Coord2 -> Integer
getCoord program (x, y) =
  head . view outputs . unsafePerformIO $ runProgram (Machine 0 [fromIntegral x, fromIntegral y] [] program 0)

followTopEdge :: Program -> Coord2 -> [Coord2]
followTopEdge program (x, y)
  | getCoord program (x, y) == 1 = (x, y) : followTopEdge program (x + 1, y)
  | otherwise = followTopEdge program (x, y + 1)

followBottomEdge :: Program -> Coord2 -> [Coord2]
followBottomEdge program (x, y)
  | getCoord program (x, y) == 1 = (x, y) : followBottomEdge program (x, y + 1)
  | otherwise = followBottomEdge program (x + 1, y)

part2 :: IO Int
part2 = do
  program <- readProgram "input/2019/19.txt"
  let ts = take 2000 $ followTopEdge program (2, 4)
      bs = take 2000 $ followBottomEdge program (2, 4)
      (x, y) =
        minimumOn manhattan0 $
          [ (bx, ty)
            | (tx, ty) <- ts,
              (bx, by) <- bs,
              tx - bx == 99,
              by - ty == 99
          ]
  return $ 10000 * x + y
