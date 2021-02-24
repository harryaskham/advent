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
