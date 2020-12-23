module TwentyTwenty.Day23 where

import Data.Char (digitToInt, intToDigit)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as M

input :: [Int]
input = digitToInt <$> "496138527"

mkNodeMap :: [Int] -> IO (M.Map Int (IORef Int))
mkNodeMap (n : ns) = do
  nodes <- M.fromList <$> mkNodeMap' (n : ns)
  cycleRef <- newIORef n
  return $ M.insert (last ns) cycleRef nodes
  where
    mkNodeMap' :: [Int] -> IO [(Int, IORef Int)]
    mkNodeMap' [_] = return []
    mkNodeMap' (n1 : n2 : s) = do
      ref <- newIORef n2
      rest <- mkNodeMap' (n2 : s)
      return $ (n1, ref) : rest

nextNode :: M.Map Int (IORef Int) -> Int -> IO Int
nextNode nexts i = readIORef (nexts M.! i)

setNextNode :: M.Map Int (IORef Int) -> Int -> Int -> IO ()
setNextNode nexts n1 n2 = writeIORef (nexts M.! n1) n2

move :: M.Map Int (IORef Int) -> Int -> IO (M.Map Int (IORef Int), Int)
move nexts c = do
  r1 <- nextNode nexts c
  r2 <- nextNode nexts r1
  r3 <- nextNode nexts r2
  r4 <- nextNode nexts r3
  setNextNode nexts c r4
  let ds' = [c - 1, c - 2, c - 3, c - 4]
      ds = (\d -> if d <= 0 then d + M.size nexts else d) <$> ds'
      d = head $ filter (not . (`elem` [r1, r2, r3])) ds
  dNext <- nextNode nexts d
  setNextNode nexts d r1
  setNextNode nexts r3 dNext
  cNext <- nextNode nexts c
  return (nexts, cNext)

readOut :: M.Map Int (IORef Int) -> Int -> [Int] -> IO [Int]
readOut nexts n sofar
  | M.size nexts == length sofar = return $ reverse sofar
  | otherwise = do
    n' <- nextNode nexts n
    readOut nexts n' (n : sofar)

moveSteps :: Int -> Int -> M.Map Int (IORef Int) -> Int -> IO (M.Map Int (IORef Int))
moveSteps steps target nexts c
  | steps == target = return nexts
  | otherwise = do
    (nexts', c') <- move nexts c
    print steps
    moveSteps (steps + 1) target nexts' c'

read2 :: M.Map Int (IORef Int) -> IO (Int, Int)
read2 nexts = do
  n1 <- nextNode nexts 1
  n2 <- nextNode nexts n1
  return (n1, n2)

longInput :: [Int]
longInput = input ++ [10 .. 1000000]

part1 :: IO String
part1 = do
  nodes <- mkNodeMap input
  moveSteps 0 100 nodes (head input)
  ans <- readOut nodes 1 []
  return $ intToDigit <$> drop 1 ans

part2 :: IO Int
part2 = do
  nodes <- mkNodeMap longInput
  nodes' <- moveSteps 0 10000000 nodes (head input)
  (n1, n2) <- read2 nodes'
  return (n1 * n2)
