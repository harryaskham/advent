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

runProgram :: Int -> ([String], V.Vector Int) -> ([String], V.Vector Int)
runProgram counter (logs, program) =
  case program V.! counter of
    99 -> (l:logs, program)
    1 -> runProgram (counter+4) (l:logs, runOp (+) (program V.! (counter + 1)) (program V.! (counter + 2)) (program V.! (counter + 3)) program)
    2 -> runProgram (counter+4) (l:logs, runOp (*) (program V.! (counter + 1)) (program V.! (counter + 2)) (program V.! (counter + 3)) program)
    _ -> error ("invalid opcode " ++ show (program V.! counter))
  where
    l = show (counter, program)

day2_1 :: IO Int
day2_1 = do
  -- Read program in as vector of ints.
  program <- V.fromList . fmap read . splitOn "," . head . lines <$> readFile "input/2019/2.txt"
  -- Make initial modifications for 1202 program and run to completion.
  let (logs, finalProgram) = runProgram 0 ([], program V.// [(1, 12), (2, 2)])
   in return $ head . V.toList $ finalProgram

day2_2 :: IO Int
day2_2 = do
  program <- V.fromList . fmap read . splitOn "," . head . lines <$> readFile "input/2019/2.txt"
  let variants = [[(1, noun), (2, verb)] | noun <- [0..99], verb <- [0..99]]
      allRuns = zip (runProgram 0 <$> [([], program V.// variant) | variant <- variants]) variants
      ((logs, finalProgram), variant) = head $ filter (\((_, p), _) -> p V.! 0 == 19690720) allRuns
   in return $ (100 * snd (head variant)) + snd (variant !! 1)
