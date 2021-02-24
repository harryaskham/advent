{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module TwentyNineteen.Solutions where

import Control.Applicative
import Control.Concurrent
import Control.Lens
import Control.Monad
import Data.Char
import Data.Foldable
import Data.Function ((&))
import Data.List
import qualified Data.List.Safe as LS
import Data.List.Split hiding (condense)
import qualified Data.Map.Strict as M
import qualified Data.Matrix as MX
import Data.Maybe
import Data.Ord
import Data.Ratio
import qualified Data.Set as S
import qualified Data.Tree as T
import qualified Data.Vector as V
import qualified Data.Vector.Split as VS
import Debug.Trace
import System.IO
import System.IO.HiddenChar
import System.Random
import Text.ParserCombinators.ReadP
import TwentyNineteen.Intcode
import Util

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
      let display'@(Display _ score) = updateDisplay (fromIntegral <$> machine' ^. outputs) display
       in do
            print score
            return $ Arcade (machine' & outputs .~ []) display'

stepArcadeI :: Arcade -> IO Arcade
stepArcadeI arcade@(Arcade machine display) = do
  machine' <- stepUntilNInputs 0 machine
  if isTerminated machine'
    then return arcade
    else
      let display'@(Display _ score) =
            if length (machine ^. outputs) == 3
              then updateDisplay (fromIntegral <$> machine' ^. outputs) display
              else display
       in do
            print score
            return $ Arcade (if length (machine ^. outputs) == 3 then machine' & outputs .~ [] else machine') display'

updateDisplay :: [Int] -> Display -> Display
updateDisplay [x, y, eId] (Display d score)
  | x == (-1) && y == 0 = Display d eId
  | otherwise = Display (MX.setElem (fromId eId) (y + 1, x + 1) d) score

mkDisplay :: Int -> Int -> Display
mkDisplay x y = Display (MX.matrix y x (const EEmpty)) 0

-- | Clear the terminal screen.
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
  length [d MX.! (y, x) | y <- [1 .. MX.nrows d], x <- [1 .. MX.ncols d], d MX.! (y, x) == Block]

day13_1 :: IO ()
day13_1 = do
  program <- readProgram "input/2019/13.txt"
  let arcade = Arcade (Machine 0 [] [] program 0) (mkDisplay 50 30)
  print =<< blockCount <$> completeArcade arcade

newtype Agent = Agent Arcade

data BallDir = BRight | BLeft deriving (Show, Eq)

dbgBrk = False

getLine_ = do
  getLine
  return ()

nAhead n f = foldr (>=>) return $ replicate n f

stepArcadeUntilBallChanges :: Arcade -> IO Arcade
stepArcadeUntilBallChanges arcade@(Arcade m d) = do
  --when dbgBrk $ print "Stepping until ball changes"
  --when dbgBrk $ print $ "Current x=" ++ (show $ ballX d)
  arcade'@(Arcade _ d') <- stepArcade arcade
  --when dbgBrk $ print $ "Next x=" ++ (show $ ballX d')
  if ballX d /= ballX d'
    then -- TODO : below could be source of slowness
      stepArcade arcade' -- Need to step twice to get over the frame with ball deleted
    else stepArcadeUntilBallChanges arcade'

-- Step either until we hit floor, or paddle
-- Must runn at least one iteration
stepArcadeUntilBallHitsFloor :: Arcade -> IO Arcade
stepArcadeUntilBallHitsFloor = go 0
  where
    go n arcade@(Arcade m d)
      | isLost d = return arcade
      | otherwise = do
        --when dbgBrk $ print n
        arcade'@(Arcade _ d') <- stepArcade (Arcade (m & inputs .~ repeat 0) d)
        if n > 0
          && ( ballHeight d == Just 0
                 || (ballHeight d == Just 1 && ((abs <$> ((-) <$> ballX d <*> paddleX d)) <= Just 1))
             )
          then return arcade
          else go (n + 1) =<< stepArcade arcade'

runAgent :: RandomGen g => g -> Maybe Int -> Agent -> IO Agent
runAgent g targetX agent@(Agent arcade@(Arcade m d@(Display _ score)))
  -- If the game is over, quit out.
  | isTerminated m = return agent
  | isLost d = return agent
  -- If we didn't draw the paddle yet, proceed until we did.
  -- Should not need any input on these stages.
  | isNothing (paddleX d) || isNothing (ballX d) = do
    nextArcade <- stepArcade arcade
    runAgent g Nothing $ Agent nextArcade
  -- If there is input to consume, first consume it.
  | not . null $ m ^. inputs = do
    when dbgBrk $ print $ "Input received: " ++ show (m ^. inputs)
    --threadDelay 1000000
    --clear
    --nextArcade@(Arcade nextM nextD) <- stepArcadeUntilBallChanges arcade
    let (nextDir, g') = randomR (-1, 0) g
    nextArcade@(Arcade nextM nextD) <- stepArcadeUntilBallChanges $ Arcade (m & inputs .~ repeat nextDir) d
    --when dbgBrk $ print $ "After consuming input: " ++ show (nextM ^. inputs)
    --when dbgBrk getLine_
    --runAgent $ Agent nextArcade
    runAgent g' targetX $ Agent $ Arcade (nextM & inputs .~ []) nextD
  -- Otherwise simulate forwards and try to predict where the ball will land.
  | otherwise = do
    when dbgBrk $ print "Current:"
    --if score > 1000 then print arcade else print score
    --if score < 15000 then print score else print arcade
    print arcade
    --print score -- to beat 15561
    --threadDelay 1000000
    --clear
    -- Simulate where the ball will travel next by passing in zero input.
    -- THIS IS COSTLY. This doesn't change once the ball is in flight.
    -- So we memoize this.
    strikeX <-
      case targetX of
        Just x -> return $ Just x
        Nothing -> do
          nextArcade@(Arcade nextM nextD) <-
            stepArcadeUntilBallHitsFloor $ Arcade (m & inputs .~ repeat 0) d
          when dbgBrk $ print "Stepped:"
          when dbgBrk $ print nextArcade
          -- let ballDir = if ballX nextD > ballX d then BRight else BLeft
          --isBallDown = ballHeight nextD < ballHeight d
          -- If the ball is coming down, anticipate its spot, otherwise just track it
          --          strikeX = subtract 1 <$> if isBallDown
          --                       then case ballDir of
          --                         BRight -> (+) <$> ballX d <*> ballHeight d
          --                         BLeft -> (-) <$> ballX d <*> ballHeight d
          --                       else (+1) <$> ballX nextD
          -- Special casing to break a loop or avoid a full-field chase
          return $ case score of
            {-
            7496 -> subtract 1 <$> ballX nextD
            10807 -> subtract 1 <$> ballX nextD
            14297 -> subtract 1 <$> ballX nextD
            15171 -> (+1) <$> ballX nextD
            15358 -> (+1) <$> ballX nextD
            15449 -> (+1) <$> ballX nextD
            -}
            _ -> ballX nextD

    --when dbgBrk $ putStrLn $ "Prev X: " ++ show (ballX d)
    --when dbgBrk $ putStrLn $ "Next X: " ++ show (ballX nextD)
    --when dbgBrk $ putStrLn $ "Prev Height: " ++ show (ballHeight d)
    --when dbgBrk $ putStrLn $ "Next Height: " ++ show (ballHeight nextD)
    --when dbgBrk $ putStrLn $ "Ball direction: " ++ show ballDir
    --when dbgBrk $ putStrLn $ "Ball descending? " ++ show isBallDown
    --when dbgBrk $ putStrLn $ "Paddle X:" ++ show (paddleX d)
    --when dbgBrk $ putStrLn $ "Predicted strike X: " ++ show strikeX
    -- If we are actually under the ball, just follow it.
    -- If we are in the right place, do nothing - this avoids slicing it.
    let thresh = 0
        newInputs =
          if
              | ((-) <$> paddleX d <*> strikeX) > Just 0 -> [-1]
              | ((-) <$> paddleX d <*> strikeX) < Just 0 -> [1]
              | otherwise -> [0]
    when dbgBrk $ putStrLn $ "Inputting: " ++ show newInputs
    when dbgBrk getLine_
    -- Set the new inputs, but don't run anything.
    -- Will get enacted on the next run of the machine.
    -- We also pass through a saved target to avoid recomputation if the ball is high enough for it not to change.
    let nextTarget = if ballHeight d > Just 1 then strikeX else Nothing
    runAgent g (Just 0) $ Agent $ Arcade (m & inputs .~ newInputs) d

--runAgent nextTarget $ Agent $ Arcade (m & inputs .~ newInputs) d

runHuman :: Agent -> IO Agent
runHuman agent@(Agent arcade@(Arcade m d))
  | isTerminated m = return agent
  | isLost d = return agent
  | not . null $ m ^. inputs = do
    nextArcade <- stepArcade arcade
    runHuman $ Agent nextArcade
  | isNothing (paddleX d) = runHuman $ Agent (Arcade (m & inputs .~ [0]) d)
  | isNothing (ballX d) = runHuman =<< Agent <$> stepArcade arcade
  | otherwise = do
    print arcade
    hSetBuffering stdin NoBuffering
    c <- getHiddenChar
    let newInputs = case c of
          'j' -> [-1]
          'k' -> [0]
          'l' -> [1]
    runHuman $ Agent (Arcade (m & inputs .~ newInputs) d)

findMx :: (Eq a) => a -> MX.Matrix a -> Maybe (Int, Int)
findMx a m = if null matches then Nothing else Just (head matches)
  where
    matches = [(y, x) | y <- [1 .. MX.nrows m], x <- [1 .. MX.ncols m], m MX.! (y, x) == a]

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
  (Agent (Arcade _ (Display _ score))) <- runAgent (mkStdGen 45) Nothing agent
  --(Agent (Arcade _ (Display _ score))) <- runHuman agent
  print score

rdbg = False

traceIf b x y = if b then trace x y else y

type Chemical = String

type Quantity = Integer

data Reaction = Reaction [(Chemical, Quantity)] (Chemical, Quantity) deriving (Show)

parseReaction :: String -> Reaction
parseReaction line = Reaction inputs' (outputChemical, read outputQuantity)
  where
    [inputLine, outputLine] = splitOn " => " line
    [outputQuantity, outputChemical] = words outputLine
    inputs = words <$> splitOn ", " inputLine
    inputs' = (\[q, c] -> (c, read q)) <$> inputs

-- Create map from chemical to the resources it needs and its yield
reactionMap :: [Reaction] -> M.Map Chemical Reaction
reactionMap = foldl' (\acc r@(Reaction _ (c, _)) -> M.insert c r acc) M.empty

ore :: M.Map Chemical Reaction -> [(Chemical, Quantity)] -> M.Map Chemical Quantity -> M.Map Chemical Quantity
ore _ [] haves = haves
ore rm (("ORE", q) : needs) haves = ore rm needs $ M.insertWith (+) "ORE" q haves
ore rm ((c, q) : needs) haves = ore rm (needs ++ multipliedReqs) newHaves
  where
    (Reaction requirements (_, outputQ)) = unjust $ M.lookup c rm
    alreadyHave = fromMaybe 0 $ M.lookup c haves
    additionalNeed = q - alreadyHave
    multiplier = ceiling (fromIntegral additionalNeed / fromIntegral outputQ)
    multipliedReqs = (fmap . fmap) (* multiplier) requirements
    actualOutput = multiplier * outputQ
    surplus = actualOutput - q
    newHaves = M.insertWith (+) c surplus haves

oreNeeded :: M.Map Chemical Reaction -> Quantity -> Quantity
oreNeeded rm fuel = unjust $ M.lookup "ORE" $ ore rm [("FUEL", fuel)] M.empty

search :: M.Map Chemical Reaction -> Quantity -> Quantity -> IO Quantity
search rm lower upper =
  if oreNeeded rm midpoint > 1000000000000
    then print midpoint >> search rm lower midpoint
    else print midpoint >> search rm midpoint upper
  where
    midpoint = (lower + upper) `div` 2

day14 :: IO ()
day14 = do
  --ls <- lines <$> readFile "input/2019/14_example.txt"
  ls <- lines <$> readFile "input/2019/14.txt"
  --ls <- lines <$> readFile "input/2019/14_example2.txt"
  --ls <- lines <$> readFile "input/2019/14_example3.txt"
  --ls <- lines <$> readFile "input/2019/14_example4.txt"
  --ls <- lines <$> readFile "input/2019/14_example5.txt"
  let reactions = parseReaction <$> ls
      rMap = reactionMap reactions
  print $ M.lookup "ORE" $ ore rMap [("FUEL", 1)] M.empty
  _ <- search rMap 1 10000000000
  return ()

errorCode :: Int -> Int -> [Integer]
errorCode l 0 = take l $ tail $ cycle [0, 1, 0, -1]
errorCode l n = take l $ tail . cycle $ concat $ replicate (n + 1) <$> [0, 1, 0, -1]

combineCodeWithNumber :: [Integer] -> [Integer] -> Integer
combineCodeWithNumber code number = fromIntegral . digitToInt . last . show $ sum mulPairs
  where
    pairs = zip code number
    mulPairs = ((*) <$> fst <*> snd) <$> pairs

runPhase :: [Integer] -> [Integer]
runPhase number = take (length number) $ (combineCodeWithNumber <$> fst <*> snd) <$> codeNumbers
  where
    codes = errorCode (length number) <$> [0 ..]
    codeNumbers = zip codes (repeat number)

runPhases :: Int -> [Integer] -> [Integer]
runPhases 0 number = number
runPhases n number = runPhases (n -1) (runPhase number)

-- Okay, need to be smarter here.
-- We need to drop 5970807 digits off the 100-phased number
-- But each digit depends on all others at each phase
-- But, these digits run in a cycle, even though the error code doesn't
-- We can find the error number for the interesting digits easily.
-- So to create digit 5970808 we need errorCode 5970807 over the entire 10000-replication number
-- We can do this once, but how to do this over the next phase?
--
-- Recognise that replication == cancelling out.
-- Any -1 in the same spot as a +1 cancels out and 0 doesn't contribute
-- But +1 must be in the same alignment as the -1, can;t just cancel 1 and -1
--
-- The last digit always remains the same!
-- The last half could be computed incrementally
-- But that's still on a length of 6,500,000 ... which isn't that bad
-- We are looking for 5970807, so well into the second half
-- And the second half never depend on the first half.
-- So we can work backwards from the last digit until we reach our target digit, compute it, and that's a phase
-- Run again 100 times and read out the first 8

-- Build the new number simply by starting with the last and adding the previous.
runEfficientPhase :: [Integer] -> [Integer]
runEfficientPhase = tail . scanl' sumD 0

sumD :: Integer -> Integer -> Integer
sumD a b = fromIntegral . digitToInt . last $ show (a + b)

runEfficientPhases :: Int -> [Integer] -> [Integer]
runEfficientPhases 0 number = number
runEfficientPhases n number = runEfficientPhases (n -1) (runEfficientPhase number)

offset :: [Integer] -> Int
offset number = read $ intToDigit . fromInteger <$> take 7 number

-- Get from a list in a cyclic fashion
modGet :: Int -> [Integer] -> Integer
modGet n xs = xs !! (n `mod` length xs)

-- Get the last N thigns "efficiently"
-- Better if we use a Vector or Array.
efficientLastN :: Int -> [Integer] -> [Integer]
efficientLastN n xs = modGet <$> [length xs - n .. length xs - 1] <*> pure xs

day16 :: IO ()
day16 = do
  ls <- lines <$> readFile "input/2019/16.txt"
  let number = fromIntegral . digitToInt <$> head ls
      target = 5970807
      bigNumber = concat $ replicate 10000 number
      lastNumber = drop target bigNumber -- efficientLastN (length bigNumber - target) bigNumber
      --print $ take 8 $ runPhases 100 number
  print $ take 8 . reverse $ runEfficientPhases 100 (reverse lastNumber)
