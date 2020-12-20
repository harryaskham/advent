module TwentyTwenty.Day21 where

inputPath :: String
inputPath = "input/2020/21.txt"

part1 :: IO Int
part1 = do
  ls <- lines <$> readFile inputPath
  return 0
