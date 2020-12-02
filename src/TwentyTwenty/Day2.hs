module TwentyTwenty.Day2 where

import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M

inputPath :: String
inputPath = "input/2020/2.txt"

data Policy = Policy Int Int Char deriving (Show)

type Password = String

parseLine :: String -> (Policy, Password)
parseLine l =
  let [policyStr, password] = splitOn ": " l
      [lowHighStr, policyChar] = splitOn " " policyStr
      [low, high] = splitOn "-" lowHighStr
   in (Policy (read low) (read high) (head policyChar), password)

isValid :: Policy -> Password -> Bool
isValid (Policy low high c) password =
  case M.lookup c charCounts of
    Just count -> count >= low && count <= high
    Nothing -> False
  where
    charCounts = M.fromListWith (+) (zip password (repeat 1))

part1 :: IO ()
part1 = do
  policyPasses <- fmap parseLine . lines <$> readFile inputPath
  print $ length $ filter (uncurry isValid) policyPasses

isValid2 :: Policy -> Password -> Bool
isValid2 (Policy i1 i2 c) password =
  (password !! (i1 - 1) == c) /= (password !! (i2 - 1) == c)

part2 :: IO ()
part2 = do
  policyPasses <- fmap parseLine . lines <$> readFile inputPath
  print $ length $ filter (uncurry isValid2) policyPasses
