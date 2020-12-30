module TwentySixteen.Day17 where

inputPath :: String
inputPath = "input/2016/17.txt"

part1 :: IO Int
part1 = do
  ls <- lines <$> readFile inputPath
  return 0
