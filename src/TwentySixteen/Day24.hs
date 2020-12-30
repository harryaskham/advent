module TwentySixteen.Day24 where

inputPath :: String
inputPath = "input/2016/24.txt"

part1 :: IO Int
part1 = do
  ls <- lines <$> readFile inputPath
  return 0
