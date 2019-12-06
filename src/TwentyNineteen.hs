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

data Opcode = Add | Mul | Input | Output | JumpIfTrue | JumpIfFalse | LessThan | Equals | Terminate deriving (Show)
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

runInstruction :: Counter -> Opcode -> [Mode] -> [Param] -> Program -> IO (Program, Counter)
runInstruction counter opcode modes params program = do
  putStrLn $ show opcode ++ show params ++ show modes
  print program
  --getLine
  case opcode of
    Terminate -> return (program, counter)
    Add -> return (program V.// [(writebackLocation, head paramVals + (paramVals !! 1))], counter + 4)
    Mul -> return (program V.// [(writebackLocation, head paramVals * (paramVals !! 1))], counter + 4)
    Input -> do
      putStrLn "Input: "
      inputVal <- getLine
      return (program V.// [(writebackLocation, read inputVal)], counter + 2)
    Output -> do
      putStrLn $ "Output: " ++ show (head paramVals)
      return (program, counter + 2)
    JumpIfTrue -> case head paramVals of
                    0 -> return (program, counter + 3)
                    _ -> return (program, paramVals !! 1)
    JumpIfFalse -> case head paramVals of
                    0 -> return (program, paramVals !! 1)
                    _ -> return (program, counter + 3)
    LessThan -> if head paramVals < paramVals !! 1 then
                  return (program V.// [(writebackLocation, 1)], counter + 4) else
                  return (program V.// [(writebackLocation, 0)], counter + 4)
    Equals -> if head paramVals == paramVals !! 1 then
                  return (program V.// [(writebackLocation, 1)], counter + 4) else
                  return (program V.// [(writebackLocation, 0)], counter + 4)
  where
    paramVal (param, mode) = case mode of
                               Immediate -> param
                               Positional -> program V.! param
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

runProgram :: Int -> Program -> IO Program
runProgram counter program = case opcode of
                                 Terminate -> do
                                   putStrLn "terminating"
                                   pure program
                                 _ -> do
                                   (nextProgram, nextCounter) <- runInstruction counter opcode modes params program
                                   runProgram nextCounter nextProgram
  where
    (opcode, modes) = parseOpcode $ program V.! counter
    params = V.toList $ V.slice (counter + 1) (numParams opcode) program

day5 :: IO ()
day5 = do
  program <- V.fromList . fmap read . splitOn "," . head . lines <$> readFile "input/2019/5.txt"
  --program <- pure . V.fromList $ [1002,4,3,4,33] -- Should write 99 to end then stop.
  runProgram 0 program
  return ()

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
