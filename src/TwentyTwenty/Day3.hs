module TwentyTwenty.Day3 where

inputPath :: String
inputPath = "input/2020/3.txt"

part1 :: IO ()
part1 = do
  ls <- lines <$> readFile inputPath
  print ls
