module TwentySixteen.Day16 where

inputPath :: String
inputPath = "input/2016/16.txt"

part1 :: IO Int
part1 = do
  ls <- lines <$> readFile inputPath
  return 0