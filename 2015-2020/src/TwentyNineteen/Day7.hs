module TwentyNineteen.Day7 where

import Control.Lens ((%~), (.~), (^.))
import Data.Function ((&))
import Data.List (permutations)
import qualified Data.Vector as V
import TwentyNineteen.Intcode
  ( Machine (Machine),
    Program,
    inputs,
    isBlocked,
    isTerminated,
    outputs,
    readProgram,
    runProgram,
    stepProgram,
  )

runPhaseConfiguration :: [Integer] -> Integer -> Program -> IO Integer
runPhaseConfiguration [] lastOutput _ = return lastOutput
runPhaseConfiguration (p : ps) lastOutput program = do
  machine <- runProgram $ Machine 0 [p, lastOutput] [] program 0
  runPhaseConfiguration ps (head $ machine ^. outputs) program

part1 :: IO Integer
part1 = do
  program <- readProgram "input/2019/7.txt"
  allOutputs <- sequenceA $ runPhaseConfiguration <$> permutations [0 .. 4] <*> [0] <*> [program]
  return $ maximum allOutputs

-- A cluster is a series of machines that can talk to one another.
-- Stores the index of the currently running machine
data Cluster = Cluster Int (V.Vector Machine)

-- A cluster is terminated once all its machines are.
isClusterTerminated :: Cluster -> Bool
isClusterTerminated (Cluster i ms) = V.all isTerminated ms

-- Run the current cluster for one step.
stepCluster :: Cluster -> IO Cluster
stepCluster (Cluster i ms) =
  if isBlocked currentMachine
    then -- If blocked, copy the output of this machine to the input of the next
    -- Kill the output of this machine and resume on the next machine

      return $
        Cluster
          nextIndex
          ( ms
              V.// [ (i, currentMachine & outputs .~ []),
                     (nextIndex, nextMachine & inputs %~ (++ currentMachine ^. outputs))
                   ]
          )
    else do
      -- If not blocked then keep running the current machine
      steppedCurrent <- stepProgram currentMachine
      return $ Cluster i (ms V.// [(i, steppedCurrent)])
  where
    currentMachine = ms V.! i
    nextIndex = (i + 1) `mod` length ms
    nextMachine = ms V.! nextIndex

-- Run a cluster until it terminates.
runCluster :: Cluster -> IO Cluster
runCluster cluster =
  if isClusterTerminated cluster
    then return cluster
    else do
      nextCluster <- stepCluster cluster
      runCluster nextCluster

-- Creates a cluster to run the given phase combination.
makeCluster :: [Integer] -> Program -> Cluster
makeCluster phases program = Cluster 0 machinesWithInput
  where
    machines = V.fromList $ (\phase -> Machine 0 [phase] [] program 0) <$> phases
    firstMachine = machines V.! 0
    machinesWithInput = machines V.// [(0, firstMachine & inputs %~ (++ [0]))]

-- Gets the final output of the cluster.
getClusterOutput :: Cluster -> Integer
getClusterOutput (Cluster _ ms) = head $ (ms V.! (V.length ms - 1)) ^. outputs

part2 :: IO Integer
part2 = do
  program <- readProgram "input/2019/7.txt"
  allCompletedClusters <- sequenceA $ runCluster <$> (makeCluster <$> permutations [5 .. 9] <*> [program])
  return $ maximum (getClusterOutput <$> allCompletedClusters)
