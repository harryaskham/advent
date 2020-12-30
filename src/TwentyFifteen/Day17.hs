module TwentyFifteen.Day17 where

inputPath :: String
inputPath = "input/2015/17.txt"

part1 :: IO Int
part1 = do
  ls <- lines <$> readFile inputPath
  return 0
