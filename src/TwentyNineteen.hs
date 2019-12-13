{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiWayIf #-}

module TwentyNineteen where

import qualified Data.Set as S
import Data.List
import Data.Char
import Data.List.Split
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Function ((&))
import Control.Applicative
import qualified Data.Vector as V
import qualified Data.Tree as T
import qualified Data.List.Safe as LS
import Control.Monad
import Data.Ord
import Control.Lens
import qualified Data.Vector.Split as VS
import Data.Ratio
import Data.Foldable
import Text.ParserCombinators.ReadP
import Debug.Trace
import Data.Matrix as MX
import System.IO
import System.IO.HiddenChar

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

data Opcode = Add
            | Mul
            | Input 
            | Output 
            | JumpIfTrue 
            | JumpIfFalse 
            | LessThan 
            | Equals 
            | AdjustBase 
            | Terminate deriving (Show, Eq)
data Mode = Positional
          | Immediate
          | Relative deriving (Show, Eq)
type Param = Integer
type Program = M.Map Integer Integer
type Counter = Integer

numParams :: Opcode -> Integer
numParams Add = 3
numParams Mul = 3
numParams Input = 1
numParams Output = 1
numParams JumpIfTrue = 2
numParams JumpIfFalse = 2
numParams LessThan = 3
numParams Equals = 3
numParams AdjustBase = 1
numParams Terminate = 0

-- The state of a machine.
data Machine = Machine { _counter :: Counter
                       , _inputs :: [Integer]
                       , _outputs :: [Integer]
                       , _program :: Program
                       , _relBase :: Integer
                       } deriving (Show)

makeLenses ''Machine

-- Wrap memory accesses so that previously unused locations are 0
unsafeMemAccess (Just a) = a
unsafeMemAccess Nothing = 0

runInstruction :: Opcode -> [Mode] -> [Param] -> Machine -> Machine
runInstruction opcode modes params machine =
  case opcode of
    Terminate -> machine
    Add -> machine & program %~ M.insert writebackLocation (head paramVals + (paramVals !! 1))
                   & counter %~ (+4)
    Mul -> machine & program %~ M.insert writebackLocation (head paramVals * (paramVals !! 1))
                   & counter %~ (+4)
    Input -> machine & program %~ M.insert writebackLocation (head $ machine ^. inputs)
                     & inputs %~ tail
                     & counter %~ (+2)
    Output -> machine & outputs %~ (++[head paramVals])
                      & counter %~ (+2)
    JumpIfTrue -> case head paramVals of
                    0 -> machine & counter %~ (+3)
                    _ -> machine & counter .~ (paramVals !! 1)
    JumpIfFalse -> case head paramVals of
                    0 -> machine & counter .~ (paramVals !! 1)
                    _ -> machine & counter %~ (+3)
    LessThan -> if head paramVals < paramVals !! 1 then
                  machine & program %~ M.insert writebackLocation 1
                          & counter %~ (+4) else
                  machine & program %~ M.insert writebackLocation 0
                          & counter %~ (+4)
    Equals -> if head paramVals == paramVals !! 1 then
                  machine & program %~ M.insert writebackLocation 1
                          & counter %~ (+4) else
                  machine & program %~ M.insert writebackLocation 0
                          & counter %~ (+4)
    AdjustBase -> machine & relBase %~ (+head paramVals)
                          & counter %~ (+2)
  where
    paramVal (param, mode) = case mode of
                               Immediate -> param
                               Positional -> unsafeMemAccess $ M.lookup param (machine ^. program)
                               Relative -> unsafeMemAccess $ M.lookup (param + (machine ^. relBase)) (machine ^. program) 
    paramVals = paramVal <$> zip params modes
    writebackLocation = if last modes == Relative then last params + (machine ^. relBase) else last params

zeroPadTo :: Int -> String -> String
zeroPadTo l x = replicate (l - length x) '0' ++ x

toMode :: Char -> Mode
toMode '0' = Positional
toMode '1' = Immediate
toMode '2' = Relative
toMode e = error $ "Invalid mode: " ++ [e]

opFromStr :: String -> Opcode
opFromStr "01" = Add
opFromStr "02" = Mul
opFromStr "03" = Input
opFromStr "04" = Output
opFromStr "05" = JumpIfTrue
opFromStr "06" = JumpIfFalse
opFromStr "07" = LessThan
opFromStr "08" = Equals
opFromStr "09" = AdjustBase
opFromStr "99" = Terminate
opFromStr s = error s

-- Parses out the opcode and the modes.
parseOpcode :: Integer -> (Opcode, [Mode])
parseOpcode x =
  ( opcode
  , toMode
    <$> reverse (take (fromIntegral $ numParams opcode)
    $ zeroPadTo (fromIntegral $ numParams opcode + 2) opStr)
  )
  where
    opStr = show x
    opcode = opFromStr $ zeroPadTo 2 $ drop (length opStr - 2) opStr

getCurrentOp :: Machine -> (Opcode, [Mode])
getCurrentOp machine = parseOpcode $ unsafeMemAccess $ M.lookup (machine ^. counter) (machine ^. program) 

stepProgram :: Machine -> IO Machine
stepProgram machine =
  {-
  do
    putStrLn $ "Counter: " ++ show (machine ^. counter)
    putStrLn $ "RelBase: " ++ show (machine ^. relBase)
    putStrLn $ "Op/Params/Modes:" ++ show opcode ++ show params ++ show modes
    putStrLn $ "In/Out:" ++ show (machine ^. inputs) ++ show (machine ^. outputs)
    print (machine ^. program)
    getLine
  -}
  return $ runInstruction opcode modes params machine
  where
    (opcode, modes) = getCurrentOp machine
    params = catMaybes
      $ M.lookup
      <$> [(machine ^. counter) + 1 .. (machine ^. counter) + numParams opcode]
      <*> [machine ^. program]

runProgram :: Machine -> IO Machine
runProgram machine = if isTerminated machine
                        then pure machine
                        else runProgram =<< stepProgram machine

day5 :: IO ()
day5 = do
  program <- readProgram "input/2019/5.txt"
  machine <- runProgram $ Machine 0 [1] [] program 0
  machine' <- runProgram $ Machine 0 [5] [] program 0
  print $ machine ^. outputs
  print $ machine' ^. outputs

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

runPhaseConfiguration :: [Integer] -> Integer -> Program -> IO Integer
runPhaseConfiguration [] lastOutput _ = return lastOutput
runPhaseConfiguration (p:ps) lastOutput program = do
  print $ "Running phase " ++ show p
  machine <- runProgram $ Machine 0 [p, lastOutput] [] program 0
  runPhaseConfiguration ps (head $ machine ^. outputs) program

readProgram :: String -> IO Program
readProgram path = do
  program <- fmap read . splitOn "," . head . lines <$> readFile path
  return $ M.fromList $ zip [0..toInteger (length program - 1)] program

day7_1 :: IO ()
day7_1 = do
  program <- readProgram "input/2019/7.txt"
  allOutputs <- sequenceA $ runPhaseConfiguration <$> permutations [0..4] <*> [0] <*> [program]
  print $ maximum allOutputs

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
makeCluster :: [Integer] -> Program -> Cluster
makeCluster phases program = Cluster 0 machinesWithInput
  where
    machines = V.fromList $ (\phase -> Machine 0 [phase] [] program 0) <$> phases
    firstMachine = machines V.! 0
    machinesWithInput = machines V.// [(0, firstMachine & inputs %~ (++[0]))]

-- Gets the final output of the cluster.
getClusterOutput :: Cluster -> Integer
getClusterOutput (Cluster _ ms) = head $ (ms V.! (V.length ms - 1)) ^. outputs

day7_2 :: IO ()
day7_2 = do
  program <- readProgram "input/2019/7.txt"
  allCompletedClusters <- sequenceA $ runCluster <$> (makeCluster <$> permutations [5..9] <*> [program])
  print $ maximum (getClusterOutput <$> allCompletedClusters)

pixelsToLayers :: Int -> Int -> [Int] -> [[Int]]
pixelsToLayers width height ps = chunksOf layerSize ps
  where
    layerSize = width * height
    numLayers = length ps `div` layerSize

numNs :: Int -> [Int] -> Int
numNs n layer = length (filter (==n) layer)

-- Go through ignoring 2s until we hit a 1 or a 0
combinePixel :: [Int] -> Int
combinePixel (2:xs) = combinePixel xs
combinePixel (1:xs) = 1
combinePixel (0:xs) = 0

stackLayers :: [[Int]] -> [Int]
stackLayers layers = combinePixel <$> stacks
  where
    stacks = [(!! i) <$> layers | i <- [0..length (head layers)]]

day8 :: IO [()]
day8 = do
  ls <- lines <$> readFile "input/2019/8.txt"
  let pixels = fmap digitToInt . head $ ls
      layers = pixelsToLayers 25 6 pixels
      layerWithFewestZeros = minimumBy (comparing $ numNs 0) layers
      stackedLayers = stackLayers layers
      image = chunksOf 25 stackedLayers
  print $ numNs 1 layerWithFewestZeros * numNs 2 layerWithFewestZeros
  sequenceA $ print <$> image

day9 :: IO ()
day9 = do
  --let xs = [1102,34915192,34915192,7,4,7,99,0]
  --let xs = [104,1125899906842624,99]
  --let xs = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
      --program = M.fromList $ zip [0..toInteger (length xs - 1)] xs
  program <- readProgram "input/2019/9.txt"
  machine1 <- runProgram $ Machine 0 [1] [] program 0
  print $ machine1 ^. outputs
  machine2 <- runProgram $ Machine 0 [2] [] program 0
  print $ machine2 ^. outputs

data Asteroid = Asteroid | NoAsteroid deriving (Eq)
type Grid = V.Vector (V.Vector Asteroid)

toAsteroid :: Char -> Asteroid
toAsteroid '#' = Asteroid
toAsteroid '.' = NoAsteroid

instance Show Asteroid where
  show Asteroid = "#"
  show NoAsteroid = "."

-- Approach:
-- All combinations of increments from (-width,-height) to (+width,+height)
-- Reduce these down to smallest integers of same ratio, remove duplicates
-- From a given asteroid, follow increments and count intersections
rayIncrements :: Grid -> [(Int, Int)]
rayIncrements grid = nub $ simplifyRatio <$> allCoords
  where
    width = V.length $ grid V.! 0
    height = V.length grid
    allCoords = [ (x, y)
                | x <- [1 - width .. width - 1]
                , y <- [1 - height .. height - 1]
                , (x, y) /= (0, 0) ]

-- Simplify e.g. (10,5) to (2,1)
simplifyRatio :: (Int, Int) -> (Int, Int)
simplifyRatio (0, 0) = (0, 0)
simplifyRatio (0, y) = (0, y `div` abs y)
simplifyRatio (x, 0) = (x `div` abs x, 0)
simplifyRatio (x, y) = preserveSign (x, y) $ (,) <$> numerator <*> denominator $ x % y

-- Ensure the ratio doesn't override signs
preserveSign (x', y') (x, y) = ( if x' == x then x else negate x
                               , if y' == y then y else negate y )

-- Count the number of rays from this position that have intersections on the grid.
numIntersections :: Grid -> (Int, Int) -> Int
numIntersections grid (x, y) =
  length . catMaybes $ intersectAsteroid grid (x, y) <$> rayIncrements grid

-- Follows the ray from the given asteroid and returns the first hit, if any.
intersectAsteroid :: Grid -> (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
intersectAsteroid grid (x, y) (incX, incY) =
  if x + incX >= 0 && x + incX < width && y + incY >= 0 && y + incY < height
    then if grid V.! (y + incY) V.! (x + incX) == Asteroid
      then Just (x + incX, y + incY)
      else intersectAsteroid grid (x + incX, y + incY) (incX, incY)
    else Nothing
  where
    width = V.length $ grid V.! 0
    height = V.length grid

asteroidLocations :: Grid -> [(Int, Int)]
asteroidLocations grid = [ (x, y)
                         | x <- [0..width-1]
                         , y <- [0..height-1]
                         , grid V.! y V.! x == Asteroid ]
  where
    width = V.length $ grid V.! 0
    height = V.length grid

-- For part 2:
-- Need to sort offsets by the angle they make
-- Then cycle through, intersecting and sploding the asteroid
-- Once we're at 200 don't splode it and instead return the coord

-- Gets the offsets sorted by angle from the upward vertical.
radialOffsets :: Grid -> [(Int, Int)]
radialOffsets grid = dropWhile (/= (0, -1)) $ cycle sortedOffsets
  where
    sortedOffsets =
      sortOn
      (Down . (\(x, y) -> atan2 (fromIntegral x) (fromIntegral y)))
      (rayIncrements grid)

explode :: (Int, Int) -> Grid -> Grid
explode (x, y) grid = grid V.// [(y, (grid V.! y) V.// [(x, NoAsteroid)])]

-- Keep exploding asteroids radially until we exploded N.
-- Return all exploded asteroids in order.
explodeUntil :: (Int, Int) -> Int -> Grid -> [(Int, Int)]
explodeUntil base n grid = reverse $ go n grid (radialOffsets grid) []
  where
    go 0 _ _ acc = acc
    go n grid (ray:rays) acc = case intersectAsteroid grid base ray of
                                 Just hit -> go (n-1) (explode hit grid) rays (hit:acc)
                                 Nothing -> go n grid rays acc

day10 :: IO ()
day10 = do
  ls <- lines <$> readFile "input/2019/10.txt"
  let grid = V.fromList $ V.fromList <$> (fmap . fmap) toAsteroid ls
      (numAsteroids, baseLoc) =
        maximumBy (comparing fst)
        $ (\loc -> (numIntersections grid loc, loc))
        <$> asteroidLocations grid
      orderedExplosions = explodeUntil baseLoc 200 grid
      (finalX, finalY) = last orderedExplosions
  print $ (100 * finalX) + finalY

data Direction = Up'
               | Down'
               | Left'
               | Right' deriving (Show)
type Position = (Int, Int)
-- A robot has a direction, its brain, its current position, and its set of whites, its set of visited
data Robot = Robot Direction Machine Position (S.Set Position) (S.Set Position) deriving (Show)

-- Proceed until we either have N outputs, or the machine terminates.
stepUntilNOutputs :: Int -> Machine -> IO Machine
stepUntilNOutputs n machine =
  if isTerminated machine || length (machine ^. outputs) == n then
    return machine else
    stepUntilNOutputs n =<< stepProgram machine

-- Proceed until we either have N inputs, or the machine terminates.
stepUntilNInputs :: Int -> Machine -> IO Machine
stepUntilNInputs n machine =
  if isTerminated machine || length (machine ^. inputs) == n then
    return machine else
    stepUntilNInputs n =<< stepProgram machine

stepRobot :: Robot -> IO Robot
stepRobot robot@(Robot direction machine position@(x,y) whites seen) = do
  let isWhite = position `S.member` whites
      currentMachine = machine & inputs %~ if isWhite then (1:) else (0:)
  nextMachine <- stepUntilNOutputs 2 currentMachine
  let 
      [colorI, rotI] = nextMachine ^. outputs
      nextWhites = case colorI of
                     0 -> S.delete position whites
                     1 -> S.insert position whites
      nextSeen = S.insert position seen
      nextDirection = case rotI of
                        0 -> case direction of
                               Up' -> Left'
                               Left' -> Down'
                               Down' -> Right'
                               Right' -> Up'
                        1 -> case direction of
                               Up' -> Right'
                               Right' -> Down'
                               Down' -> Left'
                               Left' -> Up'
      nextPosition = case nextDirection of
                       Up' -> (x,y-1)
                       Down' -> (x,y+1)
                       Left' -> (x-1,y)
                       Right' -> (x+1,y)
  if isTerminated nextMachine then
    return (Robot direction nextMachine position whites seen) else
    return $ Robot nextDirection (nextMachine & outputs .~ []) nextPosition nextWhites nextSeen

stepRobotForever :: Robot -> IO Robot
stepRobotForever robot@(Robot _ machine _ _ _) =
  if isTerminated machine then
    return robot else
    stepRobotForever =<< stepRobot robot

day11_1 :: IO ()
day11_1 = do
  program <- readProgram "input/2019/11.txt"
  let robot = Robot Up' (Machine 0 [] [] program 0) (0,0) S.empty S.empty
  (Robot _ _ _ _ seen) <- stepRobotForever robot
  print $ length seen

day11_2 :: IO ()
day11_2 = do
  program <- readProgram "input/2019/11.txt"
  let robot = Robot Up' (Machine 0 [] [] program 0) (0,0) (S.singleton (0, 0)) S.empty
  (Robot _ _ _ whites _) <- stepRobotForever robot
  sequenceA_ $ print <$> [ [ if (x, y) `S.member` whites then 'X' else ' '
                           | x <- [0..(maximum $ S.map fst whites)] ]
                         | y <- [0..(maximum $ S.map snd whites)] ]

data Body = Body { _bodyId :: Int
                 , _position :: (Int, Int, Int)
                 , _velocity :: (Int, Int, Int)
                 } deriving (Eq, Show)

makeLenses ''Body

bodies = [ Body 0 (-17, 9, -5) (0, 0, 0)
         , Body 1 (-1, 7, 13) (0, 0, 0)
         , Body 2 (-19, 12, 5) (0, 0, 0)
         , Body 3 (-6, -6, -4) (0, 0, 0)
         ]
bodiesMap = M.fromList $ zip (view bodyId <$> bodies) bodies

testBodies = [ Body 0 (-1, -0, 2) (0, 0, 0)
             , Body 1 (2, -10, -7) (0, 0, 0)
             , Body 2 (4, -8, 8) (0, 0, 0)
             , Body 3 (3, 5, -1) (0, 0, 0)
             ]
testBodiesMap = M.fromList $ zip (view bodyId <$> testBodies) testBodies

unsafeJ (Just a) = a

pairGravity :: Lens' (Int, Int, Int) Int -> (Body, Body) -> (Body, Body)
pairGravity _n (b1, b2)
  | b1^.position._n < b2^.position._n =
    (b1&velocity._n%~(+1), b2&velocity._n%~subtract 1)
  | b1^.position._n > b2^.position._n =
    (b1&velocity._n%~subtract 1, b2&velocity._n%~(+1))
  | otherwise = (b1, b2)

pairGravityX :: (Body, Body) -> (Body, Body)
pairGravityX = pairGravity _1

pairGravityY :: (Body, Body) -> (Body, Body)
pairGravityY = pairGravity _2

pairGravityZ :: (Body, Body) -> (Body, Body)
pairGravityZ = pairGravity _3

applyGravity :: [(Body, Body) -> (Body, Body)] -> M.Map Int Body -> M.Map Int Body
applyGravity axisFns bs = overAxes
  where
    ids = fst <$> M.toList bs
    allIdPairs = [(a, b) | a <- ids, b <- ids, a < b]
    foldFn pairF acc (id1, id2) = let b1 = unsafeJ $ M.lookup id1 acc
                                      b2 = unsafeJ $ M.lookup id2 acc
                                      (b1', b2') = pairF (b1, b2)
                                   in M.insert id1 b1' . M.insert id2 b2' $ acc
    overAxes = foldl' (\acc axisFn -> foldl' (foldFn axisFn) acc allIdPairs) bs axisFns

stepBody :: Body -> Body
stepBody body = body & position._1 %~ (+ body^.velocity._1)
                     & position._2 %~ (+ body^.velocity._2)
                     & position._3 %~ (+ body^.velocity._3)

stepSim :: [(Body, Body) -> (Body, Body)] -> M.Map Int Body -> M.Map Int Body
stepSim axisFns bs = stepBody <$> applyGravity axisFns bs

kinetic :: Body -> Int
kinetic body =
  abs (body^.velocity._1) +
  abs (body^.velocity._2) +
  abs (body^.velocity._3)

potential :: Body -> Int
potential body =
  abs (body^.position._1) +
  abs (body^.position._2) +
  abs (body^.position._3)

energy :: Body -> Int
energy = (*) <$> kinetic <*> potential

stepN :: Int -> M.Map Int Body -> M.Map Int Body
stepN 0 bs = bs
stepN n bs = stepN (n-1) $ stepSim [pairGravityX, pairGravityY, pairGravityZ] bs

stepUntilReturn :: [(Body, Body) -> (Body, Body)] -> M.Map Int Body -> Int
stepUntilReturn axisFns orig = go 0 orig
  where
    go :: Int -> M.Map Int Body -> Int
    go n bs = if n > 0 && orig == bs then n else go (n+1) $ stepSim axisFns bs

day12 :: IO ()
day12 = do
  print $ sum $ energy <$> (snd <$> M.toList (stepN 1000 bodiesMap))
  print $ foldl1 lcm [ stepUntilReturn [pairGravityX] bodiesMap
                     , stepUntilReturn [pairGravityY] bodiesMap
                     , stepUntilReturn [pairGravityZ] bodiesMap
                     ]

data Entity = EEmpty | Wall | Block | Paddle | Ball deriving (Eq)
type Score = Int
data Display = Display (MX.Matrix Entity) Score
data Arcade = Arcade Machine Display

instance Show Entity where
  show EEmpty = " "
  show Wall = "|"
  show Block = "X"
  show Paddle = "_"
  show Ball = "o"

instance Show Display where
  show (Display rows score) = "Score: " ++ show score ++ "\n" ++ MX.prettyMatrix rows

fromId :: Int -> Entity
fromId 0 = EEmpty
fromId 1 = Wall
fromId 2 = Block
fromId 3 = Paddle
fromId 4 = Ball

instance Show Arcade where
  show (Arcade machine display) = show display

stepArcade :: Arcade -> IO Arcade
stepArcade arcade@(Arcade machine display) = do
  machine' <- stepUntilNOutputs 3 machine
  if isTerminated machine'
     then return arcade
     else
       let display' = updateDisplay (fromIntegral <$> machine' ^. outputs) display
        in return $ Arcade (machine' & outputs .~ []) display'

updateDisplay :: [Int] -> Display -> Display
updateDisplay [x, y, eId] (Display d score)
  | x == (-1) && y == 0 = Display d eId
  | otherwise = Display (MX.setElem (fromId eId) (y+1,x+1) d) score

mkDisplay :: Int -> Int -> Display
mkDisplay x y = Display (MX.matrix y x (const EEmpty)) 0

-- |Clear the terminal screen.
clear :: IO ()
clear = putStr "\ESC[2J"

completeArcade :: Arcade -> IO Arcade
completeArcade arcade = do
  arcade'@(Arcade m d) <- stepArcade arcade
  if isTerminated m
     then return arcade'
     else completeArcade arcade'

blockCount :: Arcade -> Int
blockCount (Arcade _ (Display d _)) =
  length [d MX.! (y,x) | y <- [1..MX.nrows d], x <- [1..MX.ncols d], d MX.! (y,x) == Block]

day13_1 :: IO ()
day13_1 = do
  program <- readProgram "input/2019/13.txt"
  let arcade = Arcade (Machine 0 [] [] program 0) (mkDisplay 50 30)
  print =<< blockCount <$> completeArcade arcade

newtype Agent = Agent Arcade

data BallDir = BRight | BLeft deriving (Show)

stepArcadeUntilBallChanges :: Arcade -> IO Arcade
stepArcadeUntilBallChanges arcade@(Arcade m d) = do
  arcade'@(Arcade _ d') <- stepArcade arcade
  print arcade
  if isJust (ballX d) && isJust (ballX d') && (ballX d /= ballX d')
     then return arcade'
     else stepArcadeUntilBallChanges arcade'

runAgent :: Agent -> IO Agent
runAgent agent@(Agent arcade@(Arcade m d))
  -- If the game is over, quit out.
  | isTerminated m = return agent
  | isLost d = return agent
  -- If we didn't draw the paddle yet, proceed until we did with a zero input
  | isNothing (paddleX d)  || isNothing (ballX d) = do
      nextArcade <- stepArcade (Arcade (m & inputs .~ [0]) d)
      runAgent $ Agent nextArcade
  -- If there is input to consume, first consume it.
  -- This is the only point where the game proceeds
--  | not . null $ m ^. inputs = do
      --print arcade
      --nextArcade <- stepArcade arcade
      --runAgent $ Agent nextArcade
  -- Otherwise simulate forwards and try to predict where the ball will land.
  | otherwise = do
      print "Current"
      print arcade
      nextArcade@(Arcade _ nextD) <- stepArcadeUntilBallChanges $ Arcade (m & inputs .~ repeat 0) d
      print "Stepped"
      print nextArcade
      let ballDir = if ballX nextD > ballX d then BRight else BLeft
          isBallDown = ballHeight nextD > ballHeight d
          -- If the ball is coming down, anticipate its spot, otherwise just track it
          strikeX = if isBallDown
                       then case ballDir of
                         BRight -> (+) <$> ballX d <*> ballHeight d
                         BLeft -> (-) <$> ballX d <*> ballHeight d
                       else ballX d
      putStrLn $ "Ball direction: " ++ show ballDir
      putStrLn $ "Ball descending? " ++ show isBallDown
      putStrLn $ "Predicted strike X: " ++ show strikeX
      getLine
      let newInputs = if | paddleX d > strikeX -> [-1]
                         | paddleX d < strikeX -> [1]
                         | paddleX d == strikeX -> [0]
      do
        -- potentially here, step until need input?
        nextArcade' <- stepArcade (Arcade (m & inputs .~ newInputs) d)
        runAgent $ Agent nextArcade'

runHuman :: Agent -> IO Agent
runHuman agent@(Agent arcade@(Arcade m d))
  | isTerminated m = return agent
  | isLost d = return agent
  | not . null $ m ^. inputs = do
      nextArcade <- stepArcade arcade
      runHuman $ Agent nextArcade
  | isNothing (paddleX d) = runHuman $ Agent (Arcade (m & inputs .~ [0]) d)
  | otherwise = do
      print arcade
      hSetBuffering stdin NoBuffering
      c <- getHiddenChar
      case c of
        'j' -> runHuman $ Agent (Arcade (m & inputs .~ [-1]) d)
        'k' -> runHuman $ Agent (Arcade (m & inputs .~ [0]) d)
        'l' -> runHuman $ Agent (Arcade (m & inputs .~ [1]) d)

findMx :: (Eq a) => a -> Matrix a -> Maybe (Int, Int)
findMx a m = if null matches then Nothing else Just (head matches)
  where
    matches = [(y,x) | y <- [1..MX.nrows m], x <- [1..MX.ncols m], m MX.! (y,x) == a]

ballX :: Display -> Maybe Int
ballX (Display d _) = snd <$> findMx Ball d

ballHeight :: Display -> Maybe Int
ballHeight (Display d _) = (-) <$> (fst <$> findMx Paddle d) <*> (fst <$> findMx Ball d)

paddleX :: Display -> Maybe Int
paddleX (Display d _) = snd <$> findMx Paddle d

isLost :: Display -> Bool
isLost (Display d _) = isJust ballY && isJust paddleY && paddleY < ballY
  where
    ballY = fst <$> findMx Ball d
    paddleY = fst <$> findMx Paddle d

day13_2 :: IO ()
day13_2 = do
  program <- readProgram "input/2019/13.txt"
  let program' = M.insert 0 2 program 
      agent@(Agent arcade) = Agent $ Arcade (Machine 0 [] [] program' 0) (mkDisplay 44 21)
  (Agent (Arcade _ (Display _ score))) <- runAgent agent
  --(Agent (Arcade _ (Display _ score))) <- runHuman agent
  print score
