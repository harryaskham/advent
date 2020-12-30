module TwentySixteen.Day15 where

inputPath :: String
inputPath = "input/2016/15.txt"

part1 :: IO Int
part1 = do
  ls <- lines <$> readFile inputPath
  return 0
