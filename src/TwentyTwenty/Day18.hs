module TwentyTwenty.Day18 where

inputPath :: String
inputPath = "input/2020/18.txt"

part1 :: IO Int
part1 = do
  ls <- lines <$> readFile inputPath
  return 0
