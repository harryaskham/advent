module TwentyNineteen.Day1 where

import Util (input)

massToFuel :: Int -> Int
massToFuel m = (m `div` 3) - 2

part1 :: IO Int
part1 = do
  masses <- fmap read . lines <$> input 2019 1
  return $ sum $ massToFuel <$> masses

massToFuelIncludingFuel :: Int -> Int
massToFuelIncludingFuel m
  | massToFuel m <= 0 = 0
  | otherwise = massToFuel m + massToFuelIncludingFuel (massToFuel m)

part2 :: IO Int
part2 = do
  masses <- fmap read . lines <$> input 2019 1
  return $ sum $ massToFuelIncludingFuel <$> masses
