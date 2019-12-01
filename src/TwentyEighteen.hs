{-# LANGUAGE FlexibleContexts #-}

module TwentyEighteen where

import qualified Data.Set as S
import Data.List
import Data.Char
import Data.List.Split
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Function ((&))
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()
import Text.ParserCombinators.ReadP
import Control.Applicative

-- Read signed ints from file.
freqsToNums :: IO [Int]
freqsToNums = do
  content <- readFile "input/2018/1.txt"
  return $ parseLine <$> lines content
    where
      parseLine :: String -> Int
      parseLine (sign:number) =
        case sign of
          '+' -> read number
          '-' -> -1 * read number

day1_1 :: IO Int
day1_1 = sum <$> freqsToNums

day1_2 :: IO Int
day1_2 = do
  nums <- cycle <$> freqsToNums
  return $ next S.empty nums 0
    where
      next :: S.Set Int -> [Int] -> Int -> Int
      next seen (n:ns) frequency =
        if frequency `S.member` seen then frequency else next (S.insert frequency seen) ns (frequency + n)

-- Get a count of unique items in a list.
itemCounts :: Ord a => [a] -> M.Map a Int
itemCounts = foldl (\acc x -> M.insertWith (+) x 1 acc) M.empty

-- Does the list contain the same thing exactly n times?
exactlyN :: Ord a => Int -> [a] -> Bool
exactlyN n xs = n `elem` (snd <$> M.toList (itemCounts xs))

day2_1 :: IO Int
day2_1 = do
  content <- readFile "input/2018/2.txt"
  return $ go 2 (lines content) * go 3 (lines content)
    where
      go :: Int -> [String] -> Int
      go n ls = length $ filter (exactlyN n) ls

-- Get the number of differences between the two strings.
editDistance :: Eq a => [a] -> [a] -> Int
editDistance xs ys = length $ filter (==False) $ fmap (uncurry (==)) (zip xs ys)

-- Gets only the items that are the same in both cases.
common :: Eq a => [a] -> [a] -> [a]
common xs ys = catMaybes $ filter (/=Nothing) $ fmap (\(x, y) -> if x == y then Just x else Nothing) (zip xs ys)

day2_2 :: IO String
day2_2 = do
  ls <- lines <$> readFile "input/2018/2.txt"
  return $ head [common x y | x <- ls, y <- ls, editDistance x y == 1]

-- The details of a fabric rectangle
data FabRect = FabRect { _id :: String
                       , _xC :: Int
                       , _yC :: Int
                       , _width :: Int
                       , _height :: Int
                       } deriving (Show)

-- Read a rectangle from #id @ x,y: wxh form.
parseFabricRect :: String -> FabRect
parseFabricRect s = FabRect idStr (read xStr) (read yStr) (read widthStr) (read heightStr)
  where
    [_:idStr, _, coordStr, dimStr] = words s
    [xStr, yStr] = splitOn "," $ delete ':' coordStr
    [widthStr, heightStr] = splitOn "x" dimStr

-- Keep track of which coords have been covered how many times.
-- Pass in the current counts for each location and the current coord to update it with.
trackOverlap :: M.Map (Int, Int) Int -> (Int, Int) -> M.Map (Int, Int) Int
trackOverlap counts coord = M.insertWith (+) coord 1 counts

coordsForRect :: FabRect -> [(Int, Int)]
coordsForRect fr = [(x, y) | x <- [_xC fr.._xC fr + _width fr - 1], y <- [_yC fr.._yC fr + _height fr - 1]]

coordCounts :: [FabRect] -> M.Map (Int, Int) Int
coordCounts frs = itemCounts coords
  where
    coords = concat $ coordsForRect <$> frs

day3_1 :: IO Int
day3_1 = do
  frs <- fmap parseFabricRect . lines <$> readFile "input/2018/3.txt"
  return $ snd <$> M.toList (coordCounts frs) & filter (>1) & length

-- Is the given rectangle non-overlapping?
isRectUnique :: M.Map (Int, Int) Int -> FabRect -> Bool
isRectUnique coordCounts rect = all (==1) counts
  where
    coords = coordsForRect rect
    counts = catMaybes $ (`M.lookup` coordCounts) <$> coords

day3_2 :: IO String
day3_2 = do
  frs <- fmap parseFabricRect . lines <$> readFile "input/2018/3.txt"
  return $ _id . head $ filter (isRectUnique (coordCounts frs)) frs

-- Month Day Hour Minute
data TS = TS Int Int Int Int deriving (Eq, Ord, Show)
type GuardId = Int
data LogLine = GuardChange GuardId
             | Asleep
             | Awake deriving (Eq, Ord, Show)

-- Parse out the raw logs into timestamps and message
parseLogSleep :: ReadP (TS, LogLine)
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
  return (TS (read month) (read day) (read hours) (read minutes), logLine)
    where
      digit = satisfy isDigit
      parseGuard = do
        string "Guard #"
        guardId <- count 2 digit <|> count 3 digit
        return $ GuardChange (read guardId)
      parseAsleep = do
        string "falls asleep"
        return Asleep
      parseAwake = do
        string "wakes up"
        return Awake

-- Split the logs into chunks that start with guard changes.
splitLogs :: [(TS, LogLine)] -> [[(TS, LogLine)]]
splitLogs = drop 1 . split (whenElt isGuard)
  where
    isGuard (_, GuardChange _) = True
    isGuard _ = False

-- Groups split logs into pairs
groupSplitLogs :: [[(TS, LogLine)]] -> [(GuardId, [(TS, LogLine)])]
groupSplitLogs = undefined

day4_1 :: IO Int
day4_1 = do
  -- Parse out the timestamps and logline, taking first match and dropped rest of string.
  ls <- sort . fmap (fst . head . readP_to_S parseLogSleep) . lines <$> readFile "input/2018/4.txt"
  print $ splitLogs ls
  return 0
