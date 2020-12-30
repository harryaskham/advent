module TwentyFifteen.Day25 where

inputPath :: String
inputPath = "input/2015/25.txt"

part1 :: IO Int
part1 = do
  ls <- lines <$> readFile inputPath
  return 0
