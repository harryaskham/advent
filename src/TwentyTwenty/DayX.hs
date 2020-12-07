module TwentyTwenty.DayX where

inputPath :: String
inputPath = "input/2020/x.txt"

part1 :: IO Int
part1 = do
  ls <- lines <$> readFile inputPath
  return 0
