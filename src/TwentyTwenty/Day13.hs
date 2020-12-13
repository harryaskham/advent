module TwentyTwenty.Day13 where

import Data.List (minimumBy)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Math.LinearEquationSolver
import Text.Read (readMaybe)

inputPath :: String
inputPath = "input/2020/13_example.txt"

data BusId = BusId Int | BusX deriving (Show, Ord, Eq)

toBusId :: String -> BusId
toBusId busId =
  case readMaybe busId of
    Just b -> BusId b
    Nothing -> BusX

type Timestamp = Int

readInput :: IO (Timestamp, [BusId])
readInput = do
  [timestamp, busIdLine] <- lines <$> readFile inputPath
  return (read timestamp, toBusId <$> splitOn "," busIdLine)

timeAfter :: Timestamp -> BusId -> Maybe (Int, Int)
timeAfter _ BusX = Nothing
timeAfter ts (BusId busId) = Just (busId, busId - (ts `rem` busId))

part1 :: IO Int
part1 = do
  (ts, busIds) <- readInput
  return $
    ((*) <$> fst <*> snd)
      (minimumBy (comparing snd) (mapMaybe (timeAfter ts) busIds))

-- Represent as -t + a*busId = busIx
busIdToEquation :: Int -> Int -> (Integer, BusId) -> [Integer]
busIdToEquation len varIx (_, BusId busId) =
  (-1) : [if i == varIx then fromIntegral busId else 0 | i <- [0 .. len - 1]]

part2 :: IO Integer
part2 = do
  (_, busIds) <- readInput
  let ixBusIds = filter ((/= BusX) . snd) (zip [0 ..] busIds)
      equations = uncurry (busIdToEquation (length ixBusIds)) <$> zip [0 ..] ixBusIds
      solutions = fst <$> ixBusIds
  Just results <- solveIntegerLinearEqs Z3 equations solutions
  return $ head results
