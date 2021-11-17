module DayX where

inputPath :: String
inputPath = "input/X.txt"

part1 :: IO ()
part1 = do
  xs <- fmap read . lines <$> readFile inputPath
  print (xs :: [Int])
