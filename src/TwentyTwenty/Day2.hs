module TwentyTwenty.Day2 where

inputPath :: String
inputPath = "input/2020/2.txt"

part1 :: IO ()
part1 = do
  ls <- lines <$> readFile inputPath
  print ls
