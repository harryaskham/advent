{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module TwentyNineteen where

import qualified Data.Set as S
import Data.List
import Data.Char
import Data.List.Split
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Function ((&))
import Text.ParserCombinators.ReadP
import Control.Applicative
import qualified Data.Vector as V
import qualified Data.Tree as T
import qualified Data.List.Safe as LS
import Control.Monad
import Control.Lens

-- Convert the given mass to basic fuel requirement.
massToFuel :: Int -> Int
massToFuel m = (m `div` 3) - 2

day1_1 :: IO Int
day1_1 = do
  masses <- fmap read . lines <$> readFile "input/2019/1.txt"
  return $ sum $ massToFuel <$> masses

-- How much fuel does the fuel itself need including the mass
massToFuelIncludingFuel :: Int -> Int
massToFuelIncludingFuel m
  | massToFuel m <= 0 = 0
  | otherwise = massToFuel m + massToFuelIncludingFuel (massToFuel m)

day1_2 :: IO Int
day1_2 = do
  masses <- fmap read . lines <$> readFile "input/2019/1.txt"
  return $ sum $ massToFuelIncludingFuel <$> masses

-- Run a single operation on the given locations.
runOp :: (Int -> Int -> Int) -> Int -> Int -> Int -> V.Vector Int -> V.Vector Int
runOp op loc1 loc2 locR program = program V.// [(locR, res)]
  where
    x1 = program V.! loc1
    x2 = program V.! loc2
    res = op x1 x2

runProgramD2 :: Int -> ([String], V.Vector Int) -> ([String], V.Vector Int)
runProgramD2 counter (logs, program) =
  case program V.! counter of
    99 -> (l:logs, program)
    1 -> runProgramD2 (counter+4) (l:logs, runOp (+) (program V.! (counter + 1)) (program V.! (counter + 2)) (program V.! (counter + 3)) program)
    2 -> runProgramD2 (counter+4) (l:logs, runOp (*) (program V.! (counter + 1)) (program V.! (counter + 2)) (program V.! (counter + 3)) program)
    _ -> error ("invalid opcode " ++ show (program V.! counter))
  where
    l = show (counter, program)

day2_1 :: IO Int
day2_1 = do
  -- Read program in as vector of ints.
  program <- V.fromList . fmap read . splitOn "," . head . lines <$> readFile "input/2019/2.txt"
  -- Make initial modifications for 1202 program and run to completion.
  let (logs, finalProgram) = runProgramD2 0 ([], program V.// [(1, 12), (2, 2)])
   in return $ head . V.toList $ finalProgram

day2_2 :: IO Int
day2_2 = do
  program <- V.fromList . fmap read . splitOn "," . head . lines <$> readFile "input/2019/2.txt"
  let variants = [[(1, noun), (2, verb)] | noun <- [0..99], verb <- [0..99]]
      allRuns = zip (runProgramD2 0 <$> [([], program V.// variant) | variant <- variants]) variants
      ((logs, finalProgram), variant) = head $ filter (\((_, p), _) -> p V.! 0 == 19690720) allRuns
   in return $ (100 * snd (head variant)) + snd (variant !! 1)

type Wire = [String]

-- Get all the coordinates that a wire occupies including how long it took to first get there in steps.
wireToCoords :: [String] -> M.Map (Int, Int) Int
wireToCoords = go 0 (0, 0) M.empty
  where
    go :: Int -> (Int, Int) -> M.Map (Int, Int) Int -> [String] -> M.Map (Int, Int) Int
    go _ _ seen [] = seen
    go step (x, y) seen ((d:n):dirs) = go (step + length segments - 1) (nextX, nextY) insertedSegments dirs
      where
        n' = read n
        (nextX, nextY) = case d of
                           'R' -> (x + n', y)
                           'L' -> (x - n', y)
                           'D' -> (x, y + n')
                           'U' -> (x, y - n')
        -- Must sort the segments by distance from current, so that we always follow a continuous path.
        segments = sortOn (mDistance (x, y)) [(x', y') | x' <- allBetween x nextX, y' <- allBetween y nextY]
        -- Convert to a map from segment to the step number on which we land on it.
        segmentSteps = M.fromList $ zip segments [step..(step + length segments)]
        -- Insert with preference for already seen keys to ensure we keep the smallest distance.
        insertedSegments = M.union seen segmentSteps

-- Get all members between, inclusive. Can provide args in any order.
allBetween :: (Enum a) => a -> a -> [a]
allBetween x y = if fromEnum x <= fromEnum y then [x..y] else [y..x]

-- Manhattan distance.
mDistance :: (Int, Int) -> (Int, Int) -> Int
mDistance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

day3_1 :: IO Int
day3_1 = do
  directions <- fmap (splitOn ",") . lines <$> readFile "input/2019/3.txt"
  let intersections = S.delete (0, 0) $ foldl1 S.intersection (S.fromList . fmap fst . M.toList . wireToCoords <$> directions)
   in return $ minimum (mDistance (0, 0) <$> S.toList intersections)

day3_2 :: IO (Maybe Int)
day3_2 = do
  directions <- fmap (splitOn ",") . lines <$> readFile "input/2019/3.txt"
  let coordMaps = wireToCoords <$> directions
      (coord1, coord2) = (head coordMaps, coordMaps !! 1)
      intersections = S.delete (0, 0) $ foldl1 S.intersection (S.fromList . fmap fst . M.toList . wireToCoords <$> directions)
      intersectionSums = sequenceA $ (\k -> (+) <$> M.lookup k coord1 <*> M.lookup k coord2) <$> S.toList intersections
   in return $ minimum <$> intersectionSums

hasTwoAdjacent :: Int -> Bool
hasTwoAdjacent x = go $ show x
  where
    go :: String -> Bool
    go [] = False
    go [_] = False
    go (a:b:xs) = (a == b) || go (b:xs)

hasMonotonicDigits :: Int -> Bool
hasMonotonicDigits x = go $ show x
  where
    go :: String -> Bool
    go [] = True
    go [_] = True
    go (a:b:xs) = (digitToInt b >= digitToInt a) && go (b:xs)

day4_1 :: Int
day4_1 = length [x | x <- input, hasTwoAdjacent x, hasMonotonicDigits x]
  where
    input = [265275..781584]

hasPreciselyTwoAdjacent :: Int -> Bool
hasPreciselyTwoAdjacent x = go $ show x
  where
    go :: String -> Bool
    go [a, b, c, d, e, f] = or [ (a == b) && (b /= c)
                               , (a /= b) && (b == c) && (c /= d)
                               , (b /= c) && (c == d) && (d /= e)
                               , (c /= d) && (d == e) && (e /= f)
                               , (d /= e) && (e == f) ]

day4_2 :: Int
day4_2 = length [x | x <- input, hasPreciselyTwoAdjacent x, hasMonotonicDigits x]
  where
    input = [265275..781584]

data Opcode = Add | Mul | Input | Output | JumpIfTrue | JumpIfFalse | LessThan | Equals | Terminate deriving (Show, Eq)
data Mode = Positional | Immediate deriving (Show)
type Param = Int
type Program = V.Vector Int
type Counter = Int

numParams :: Opcode -> Int
numParams Add = 3
numParams Mul = 3
numParams Input = 1
numParams Output = 1
numParams JumpIfTrue = 2
numParams JumpIfFalse = 2
numParams LessThan = 3
numParams Equals = 3
numParams Terminate = 0

-- The state of a machine.
data Machine = Machine { _counter :: Counter
                       , _inputs :: [Int]
                       , _outputs :: [Int]
                       , _program :: Program
                       }

makeLenses ''Machine

runInstruction :: Opcode -> [Mode] -> [Param] -> Machine -> IO Machine
runInstruction opcode modes params machine = do
  putStrLn $ show opcode ++ show params ++ show modes
  --putStrLn $ show (machine ^. inputs) ++ show (machine ^. outputs)
  --print (machine ^. program)
  --getLine
  case opcode of
    Terminate -> return machine
    Add -> return $ machine & program %~ (V.// [(writebackLocation, head paramVals + (paramVals !! 1))])
                            & counter %~ (+4)
    Mul -> return $ machine & program %~ (V.// [(writebackLocation, head paramVals * (paramVals !! 1))])
                            & counter %~ (+4)
    Input -> return $ machine & program %~ (V.// [(writebackLocation, head $ machine ^. inputs)])
                              & inputs %~ tail
                              & counter %~ (+2)
    Output -> return $ machine & outputs %~ (++[head paramVals])
                               & counter %~ (+2)
    JumpIfTrue -> case head paramVals of
                    0 -> return $ machine & counter %~ (+3)
                    _ -> return $ machine & counter .~ (paramVals !! 1)
    JumpIfFalse -> case head paramVals of
                    0 -> return $ machine & counter .~ (paramVals !! 1)
                    _ -> return $ machine & counter %~ (+3)
    LessThan -> if head paramVals < paramVals !! 1 then
                  return $ machine & program %~ (V.// [(writebackLocation, 1)])
                                   & counter %~ (+4) else
                  return $ machine & program %~ (V.// [(writebackLocation, 0)])
                                   & counter %~ (+4)
    Equals -> if head paramVals == paramVals !! 1 then
                  return $ machine & program %~ (V.// [(writebackLocation, 1)])
                                   & counter %~ (+4) else
                  return $ machine & program %~ (V.// [(writebackLocation, 0)])
                                   & counter %~ (+4)
  where
    paramVal (param, mode) = case mode of
                               Immediate -> param
                               Positional -> (machine ^. program) V.! param
    paramVals = paramVal <$> zip params modes
    writebackLocation = last params  -- Always use the exact writeback location

zeroPadTo :: Int -> String -> String
zeroPadTo l x = replicate (l - length x) '0' ++ x

toMode :: Char -> Mode
toMode '0' = Positional
toMode '1' = Immediate
toMode e = error $ "Invalid mode: " ++ [e]

-- TODO: Revisit once more than 10 ops.
opFromChar :: Char -> Opcode
opFromChar '1' = Add
opFromChar '2' = Mul
opFromChar '3' = Input
opFromChar '4' = Output
opFromChar '5' = JumpIfTrue
opFromChar '6' = JumpIfFalse
opFromChar '7' = LessThan
opFromChar '8' = Equals
opFromChar '9' = Terminate

-- Parses out the opcode and the modes.
parseOpcode :: Int -> (Opcode, [Mode])
parseOpcode x = (opcode, toMode <$> reverse (take (numParams opcode) $ zeroPadTo (numParams opcode + 2) opStr))
  where
    opStr = show x
    opcode = opFromChar $ last opStr

getCurrentOp :: Machine -> (Opcode, [Mode])
getCurrentOp machine = parseOpcode $ (machine ^. program) V.! (machine ^. counter)

stepProgram :: Machine -> IO Machine
stepProgram machine = 
  case opcode of
    Terminate -> do
      putStrLn "terminating"
      pure machine
    _ -> runInstruction opcode modes params machine
  where
    (opcode, modes) = getCurrentOp machine
    params = V.toList $ V.slice (machine ^. counter + 1) (numParams opcode) (machine ^. program)

runProgram :: Machine -> IO Machine
runProgram machine = 
  case opcode of
    Terminate -> pure machine
    _ -> do
      nextMachine <- stepProgram machine
      runProgram nextMachine
  where
    (opcode, _) = getCurrentOp machine

day5 :: IO ()
day5 = do
  program <- V.fromList . fmap read . splitOn "," . head . lines <$> readFile "input/2019/5.txt"
  machine <- runProgram $ Machine 0 [1] [] program
  print $ machine ^. outputs

data OrbitTree = OrbitTree String [OrbitTree] deriving (Show)

orbitMap :: [String] -> M.Map String [String]
orbitMap = foldl addEntry M.empty
  where
    addEntry acc orbit = M.insertWith (++) oTo [oFrom] acc
      where
        [oTo, oFrom] = splitOn ")" orbit

toTree :: M.Map String [String] -> OrbitTree
toTree om = go "COM"
  where
    go :: String -> OrbitTree
    go n = OrbitTree n (go <$> childStrings)
      where
        childStrings = fromMaybe [] $ M.lookup n om

countOrbits :: OrbitTree -> Int
countOrbits = go 0
  where
    go depth (OrbitTree _ []) = depth
    go depth (OrbitTree _ cs) = depth + sum (go (depth + 1) <$> cs)

santaDistance :: OrbitTree -> Maybe Int
santaDistance ot@(OrbitTree _ cs) =
  (-)
  <$> (LS.minimum =<< sequenceA (filter isJust (distanceFromHere:distanceFromChildren)))
  <*> Just 2
  where
    distanceTo depth target (OrbitTree n []) =
      if n == target then Just depth else Nothing
    distanceTo depth target (OrbitTree n cs) =
      if n == target then Just depth else join $ LS.head $ filter isJust children
      where
        children = distanceTo (depth + 1) target <$> cs
    distanceFromHere = (+) <$> distanceTo 0 "SAN" ot <*> distanceTo 0 "YOU" ot
    distanceFromChildren = santaDistance <$> cs

day6 :: IO ()
day6 = do
  --orbits <- lines <$> readFile "input/2019/6_example.txt"
  orbits <- lines <$> readFile "input/2019/6.txt"
  let tree = toTree . orbitMap $ orbits
   in do
     print tree
     print $ countOrbits tree
     print $ santaDistance tree

runPhaseConfiguration :: [Int] -> Int -> Program -> IO Int
runPhaseConfiguration [] lastOutput _ = return lastOutput
runPhaseConfiguration (p:ps) lastOutput program = do
  print $ "Running phase " ++ show p
  machine <- runProgram $ Machine 0 [p, lastOutput] [] program
  runPhaseConfiguration ps (head $ machine ^. outputs) program

day7_1 :: IO Int
day7_1 = do
  program <- V.fromList . fmap read . splitOn "," . head . lines <$> readFile "input/2019/7.txt"
  allOutputs <- sequenceA $ runPhaseConfiguration <$> permutations [0..4] <*> [0] <*> [program]
  return $ maximum allOutputs

-- Whether or not the machine is currently blocked from running
isBlocked :: Machine -> Bool
isBlocked machine = isTerminated machine || (opcode == Input && null (machine ^. inputs))
  where
    (opcode, _) = getCurrentOp machine

isTerminated :: Machine -> Bool
isTerminated machine = opcode == Terminate
  where
    (opcode, _) = getCurrentOp machine

-- A cluster is a series of machines that can talk to one another.
-- Stores the index of the currently running machine
data Cluster = Cluster Int (V.Vector Machine)

-- A cluster is terminated once all its machines are.
isClusterTerminated :: Cluster -> Bool
isClusterTerminated (Cluster i ms) = V.all isTerminated ms

-- Run the current cluster for one step.
stepCluster :: Cluster -> IO Cluster
stepCluster (Cluster i ms) =
  if isBlocked currentMachine then
    -- If blocked, copy the output of this machine to the input of the next
    -- Kill the output of this machine and resume on the next machine
    return $
      Cluster nextIndex (ms V.// [ (i, currentMachine & outputs .~ [])
                                 , (nextIndex, nextMachine & inputs %~ (++ currentMachine ^. outputs))
                                 ])

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
runCluster cluster = if isClusterTerminated cluster then return cluster else do
  nextCluster <- stepCluster cluster
  runCluster nextCluster

-- Creates a cluster to run the given phase combination.
makeCluster :: [Int] -> Program -> Cluster
makeCluster phases program = Cluster 0 machinesWithInput
  where
    machines = V.fromList $ (\phase -> Machine 0 [phase] [] program) <$> phases
    firstMachine = machines V.! 0
    machinesWithInput = machines V.// [(0, firstMachine & inputs %~ (++[0]))]

-- Gets the final output of the cluster.
getClusterOutput :: Cluster -> Int
getClusterOutput (Cluster _ ms) = head $ (ms V.! (V.length ms - 1)) ^. outputs

day7_2 :: IO Int
day7_2 = do
  program <- V.fromList . fmap read . splitOn "," . head . lines <$> readFile "input/2019/7.txt"
  --program <- pure $ V.fromList [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]
  allCompletedClusters <- sequenceA $ runCluster <$> (makeCluster <$> permutations [5..9] <*> [program])
  return $ maximum (getClusterOutput <$> allCompletedClusters)
