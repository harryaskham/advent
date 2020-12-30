module TwentySixteen.Day1 where

inputPath :: String
inputPath = "input/2016/1.txt"

part1 :: IO Int
part1 = do
  ls <- lines <$> readFile inputPath
  return 0
