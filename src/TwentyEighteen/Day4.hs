module TwentyEighteen.Day4 where

import Data.Char (isDigit)
import Data.List (maximumBy, sort)
import Data.List.Split (chunksOf, split, whenElt)
import qualified Data.Map.Strict as M
import Data.Ord (comparing)
import Data.Time (Day, TimeOfDay (TimeOfDay), fromGregorian)
import Text.ParserCombinators.ReadP
  ( ReadP,
    count,
    manyTill,
    readP_to_S,
    satisfy,
    string,
    (+++),
  )
import Util (countMap)

-- Month Day Hour Minute
data TimeStamp = TimeStamp Day TimeOfDay deriving (Eq, Ord, Show)

type GuardId = Int

data LogLine
  = GuardChange GuardId
  | Asleep
  | Awake
  deriving (Eq, Ord, Show)

-- Parse out the raw logs into timestamps and message
parseLogSleep :: ReadP (TimeStamp, LogLine)
parseLogSleep = do
  string "[1518-"
  month <- count 2 digit
  string "-"
  day <- count 2 digit
  string " "
  hours <- count 2 digit
  string ":"
  minutes <- count 2 digit
  string "] "
  logLine <- parseGuard +++ parseAsleep +++ parseAwake
  return (TimeStamp (fromGregorian 1518 (read month) (read day)) (TimeOfDay (read hours) (read minutes) 0), logLine)
  where
    digit = satisfy isDigit
    parseGuard = do
      string "Guard #"
      guardId <- manyTill digit (satisfy (== ' '))
      return $ GuardChange (read guardId)
    parseAsleep = do
      string "falls asleep"
      return Asleep
    parseAwake = do
      string "wakes up"
      return Awake

-- Split the logs into chunks - either only guard changes or only awake/asleep.
splitLogs :: [(TimeStamp, LogLine)] -> [[(TimeStamp, LogLine)]]
splitLogs = drop 1 . split (whenElt isGuard)
  where
    isGuard (_, GuardChange _) = True
    isGuard _ = False

-- Groups split logs into pairs
groupSplitLogs :: [[(TimeStamp, LogLine)]] -> [(GuardId, [(TimeStamp, LogLine)])]
groupSplitLogs ls = (\l -> (getGuard $ head l, l !! 1)) <$> chunksOf 2 ls
  where
    getGuard [(_, GuardChange guardId)] = guardId

-- Gets the total minutes asleep in the given awake/asleep logs.
minutesAsleep :: [(TimeStamp, LogLine)] -> Int
minutesAsleep ls = sum (sleepMinutes <$> chunks)
  where
    chunks = chunksOf 2 (fst <$> ls)
    sleepMinutes [TimeStamp _ t1, TimeStamp _ t2] = length $ getMinutes t1 t2

-- Gets the discrete minutes asleep in the given logs
discreteMinutesAsleep :: [(TimeStamp, LogLine)] -> [Int]
discreteMinutesAsleep ls = concat $ sleepMinutes <$> chunks
  where
    chunks = chunksOf 2 (fst <$> ls)
    sleepMinutes [TimeStamp _ t1, TimeStamp _ t2] = getMinutes t1 t2

-- Gets pairs of (guard ID, minutes asleep) - guard ID could be repeated.
guardMinutesAsleep :: [(GuardId, [(TimeStamp, LogLine)])] -> [(GuardId, [Int])]
guardMinutesAsleep = (fmap . fmap) discreteMinutesAsleep

-- Create a map from guard ID to list of minutes spent asleep.
guardTotals :: [(GuardId, [Int])] -> M.Map GuardId [Int]
guardTotals = foldl (\acc (guardId, minutes) -> M.insertWith (++) guardId minutes acc) M.empty

-- Find the guard with the most total sleep.
sleepiestGuard :: M.Map GuardId [Int] -> GuardId
sleepiestGuard guards = fst $ maximumBy (comparing (length . snd)) $ M.toList guards

-- Find a given guard's most common minute
mostCommonGuardMinute :: M.Map GuardId [Int] -> GuardId -> Int
mostCommonGuardMinute guards guardId = maybe 0 (fst . mostCommon) (M.lookup guardId guards)

-- Get the most common thing along with its count.
mostCommon :: (Ord a, Eq a) => [a] -> (a, Int)
mostCommon xs = maximumBy (comparing snd) $ M.toList (countMap xs)

-- Converts a timestamp pair to the list of minute-values it contains
getMinutes :: TimeOfDay -> TimeOfDay -> [Int]
getMinutes (TimeOfDay _ m1 _) (TimeOfDay _ m2 _) = [m1 .. m2 -1]

part1 :: IO Int
part1 = do
  -- Parse out the timestamps and logline, taking first match and dropped rest of string.
  ls <- sort . fmap (fst . head . readP_to_S parseLogSleep) . lines <$> readFile "input/2018/4.txt"
  let guards = guardTotals . guardMinutesAsleep . groupSplitLogs . splitLogs $ ls
      guard = sleepiestGuard guards
      minute = mostCommonGuardMinute guards guard
   in return $ guard * minute

part2 :: IO Int
part2 = do
  ls <- sort . fmap (fst . head . readP_to_S parseLogSleep) . lines <$> readFile "input/2018/4.txt"
  let guards = guardTotals . guardMinutesAsleep . groupSplitLogs . splitLogs $ ls
      guardsToMostCommonMinute = (\(g, ms) -> (g, mostCommon ms)) <$> filter (not . null . snd) (M.toList guards)
      (guard, (minute, count)) = maximumBy (comparing $ snd . snd) guardsToMostCommonMinute
   in return $ guard * minute
