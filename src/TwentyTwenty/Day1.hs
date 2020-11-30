module TwentyTwenty.Day1 where

inputPath :: Int -> String
inputPath day = "input/2020/" <> show day <> ".txt"

part1 :: IO ()
part1 = do
  ls <- lines <$> (readFile $ inputPath 1)
  print ls
