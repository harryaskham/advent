module TwentyTwenty.Day1 where

inputPath :: String
inputPath = "input/2020/1.txt"

part1 :: IO ()
part1 = do
  ls <- lines <$> readFile inputPath
  print ls
