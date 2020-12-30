module TwentyFifteen.Day9 where

inputPath :: String
inputPath = "input/2015/9.txt"

part1 :: IO Int
part1 = do
  ls <- lines <$> readFile inputPath
  return 0
