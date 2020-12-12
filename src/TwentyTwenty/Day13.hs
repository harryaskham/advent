module TwentyTwenty.Day13 where

inputPath :: String
inputPath = "input/2020/13.txt"

part1 :: IO Int
part1 = do
  ls <- lines <$> readFile inputPath
  return 0
