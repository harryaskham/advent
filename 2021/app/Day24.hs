module Day24 where

inputPath :: String
inputPath = "input/24.txt"

part1 :: IO ()
part1 = do
  xs <- fmap read . lines <$> readFile inputPath
  print (xs :: [Int])
