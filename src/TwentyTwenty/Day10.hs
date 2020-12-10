module TwentyTwenty.Day10 where

import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (sort)
import qualified Data.Map.Strict as M

inputPath :: String
inputPath = "input/2020/10.txt"

differences :: Int -> Int -> Int -> [Int] -> Int
differences _ ones threes [] = ones * (threes + 1)
differences last ones threes (j : js)
  | j - last == 1 = differences j (ones + 1) threes js
  | j - last == 3 = differences j ones (threes + 1) js
  | otherwise = error "Impossible input"

part1 :: IO Int
part1 = differences 0 0 0 . sort . fmap read . lines <$> readFile inputPath

arrangements :: IORef (M.Map (Int, Int) Int) -> Int -> [Int] -> IO Int
arrangements _ _ [_] = return 1
arrangements dRef last (j1 : j2 : js) = do
  let get last = do
        d <- readIORef dRef
        case M.lookup (last, j2) d of
          Just n -> return n
          Nothing -> do
            n <- arrangements dRef last (j2 : js)
            modifyIORef' dRef (M.insert (last, j2) n)
            return n
  nWithoutSkip <- get j1
  nWithSkip <- get last
  if j2 - last > 3
    then return nWithoutSkip
    else return (nWithoutSkip + nWithSkip)

part2 :: IO Int
part2 = do
  js <- sort . fmap read . lines <$> readFile inputPath
  dRef <- newIORef M.empty
  arrangements dRef 0 js
