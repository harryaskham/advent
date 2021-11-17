module TwentyNineteen.Day23 where

import Control.Lens (over, set, view)
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Tuple.Extra (fst3)
import System.IO.Unsafe (unsafePerformIO)
import TwentyNineteen.Intcode
  ( Machine (Machine),
    inputs,
    outputs,
    readProgram,
    stepProgram,
  )
import Util (toTuple3)

type Network = Map Integer Machine

stepNetwork :: Network -> Integer
stepNetwork network =
  case [y | (i, _, y) <- packets, i == 255] of
    [] -> stepNetwork $ unsafePerformIO . stepProgram <$> network'
    y : _ -> y
  where
    packets =
      [ toTuple3 os
        | m <- M.elems network,
          let os = view outputs m,
          length os == 3
      ]
    replaceEmpty m
      | null (view inputs m) = set inputs [-1] m
      | otherwise = m
    replaceThrees m
      | length (view outputs m) == 3 = set outputs [] m
      | otherwise = m
    receivePacket (x, y) m = over inputs (++ [x, y]) m
    network' =
      replaceThrees
        . replaceEmpty
        <$> foldl'
          (\acc (i, x, y) -> M.adjust (receivePacket (x, y)) i acc)
          network
          packets

solve :: (Network -> Integer) -> IO Integer
solve f = do
  program <- readProgram "input/2019/23.txt"
  let network = M.fromList [(i, Machine 0 [i] [] program 0) | i <- [0 .. 49]]
  return $ f network

part1 :: IO Integer
part1 = solve stepNetwork

stepNetworkNAT :: Int -> Set Integer -> Maybe (Integer, Integer) -> Network -> Integer
stepNetworkNAT timeSinceLastPacket seen nat network
  | timeSinceLastPacket > 1000 =
    let Just (x, y) = nat'
     in if y `S.member` seen
          then y
          else
            stepNetworkNAT
              0
              (S.insert y seen)
              nat'
              (M.adjust (receivePacket (x, y)) 0 network')
  | otherwise =
    stepNetworkNAT
      timeSinceLastPacket'
      seen
      nat'
      (unsafePerformIO . stepProgram <$> network')
  where
    packets =
      [ toTuple3 os
        | m <- M.elems network,
          let os = view outputs m,
          length os == 3
      ]
    timeSinceLastPacket' = case packets of
      [] -> timeSinceLastPacket + 1
      _ -> 0
    machinePackets = filter ((/= 255) . fst3) packets
    natPackets = filter ((== 255) . fst3) packets
    nat' = case reverse natPackets of
      [] -> nat
      (_, x, y) : _ -> Just (x, y)
    replaceEmpty m
      | null (view inputs m) = set inputs [-1] m
      | otherwise = m
    replaceThrees m
      | length (view outputs m) == 3 = set outputs [] m
      | otherwise = m
    receivePacket (x, y) m = over inputs (++ [x, y]) m
    network' =
      replaceThrees
        . replaceEmpty
        <$> foldl'
          (\acc (i, x, y) -> M.adjust (receivePacket (x, y)) i acc)
          network
          machinePackets

part2 :: IO Integer
part2 = solve (stepNetworkNAT 0 S.empty Nothing)
