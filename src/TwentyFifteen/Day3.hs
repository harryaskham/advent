module TwentyFifteen.Day3 where

inputPath :: String
inputPath = "input/2015/3.txt"

part1 :: IO Int
part1 = do
  ls <- lines <$> readFile inputPath
  return 0
