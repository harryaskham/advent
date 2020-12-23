module TwentyTwenty.Day23 where

import Control.Monad.State
import Data.Char (digitToInt, intToDigit)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.IntMap as IM
import qualified Data.Map.Strict as M
import Debug.Trace

input :: [Int]
input = digitToInt <$> "496138527"

mkNodeMap :: [Int] -> IM.IntMap Int
mkNodeMap ns = IM.fromList $ (last ns, head ns) : mkNodeMap' ns
  where
    mkNodeMap' :: [Int] -> [(Int, Int)]
    mkNodeMap' [_] = []
    mkNodeMap' (n1 : n2 : s) = (n1, n2) : mkNodeMap' (n2 : s)

nextNode :: Int -> State (IM.IntMap Int) Int
nextNode i = gets (IM.! i)

setNextNode :: Int -> Int -> State (IM.IntMap Int) ()
setNextNode n1 n2 = modify (IM.insert n1 n2)

move :: Int -> State (IM.IntMap Int) Int
move c = do
  size <- gets IM.size
  r1 <- nextNode c
  r2 <- nextNode r1
  r3 <- nextNode r2
  r4 <- nextNode r3
  setNextNode c r4
  let ds' = [c - 1, c - 2, c - 3, c - 4]
      ds = (\d -> if d <= 0 then d + size else d) <$> ds'
      d = head $ filter (not . (`elem` [r1, r2, r3])) ds
  dNext <- nextNode d
  setNextNode d r1
  setNextNode r3 dNext
  cNext <- nextNode c
  return cNext

readOut :: Int -> [Int] -> State (IM.IntMap Int) [Int]
readOut n sofar = do
  size <- gets IM.size
  if length sofar == size
    then return $ reverse sofar
    else do
      n' <- nextNode n
      readOut n' (n : sofar)

moveSteps :: Int -> Int -> Int -> State (IM.IntMap Int) ()
moveSteps steps target c
  | steps == target = return ()
  | otherwise = do
    c' <- move c
    trace (show (steps, c)) $ moveSteps (steps + 1) target c'

longInput :: [Int]
longInput = input ++ [10 .. 1000000]

part1 :: String
part1 = flip evalState (mkNodeMap input) $ do
  moveSteps 0 100 (head input)
  ans <- readOut 1 []
  return $ intToDigit <$> drop 1 ans

part2 :: Int
part2 = flip evalState (mkNodeMap longInput) $ do
  moveSteps 0 10000000 (head longInput)
  n1 <- nextNode 1
  n2 <- nextNode n1
  return (n1 * n2)
