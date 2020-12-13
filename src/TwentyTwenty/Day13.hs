module TwentyTwenty.Day13 where

import Data.List (foldl', minimumBy)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Text.Read (readMaybe)

inputPath :: String
inputPath = "input/2020/13.txt"

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

findNext :: (Int, Int) -> (Int, BusId) -> (Int, Int)
findNext (current, step) (offset, BusId busId) =
  if (current + offset) `mod` busId == 0
    then (current, step * busId)
    else findNext (current + step, step) (offset, BusId busId)

part2 :: IO Int
part2 = do
  (_, busIds) <- readInput
  let ixBusIds = filter ((/= BusX) . snd) (zip [0 ..] busIds)
  return . fst $ foldl' findNext (1, 1) ixBusIds
