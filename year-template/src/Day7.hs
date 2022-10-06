module Day7 (part1, part2) where

import Data.List (maximum, minimum)
import Helper.TH (input)
import Helper.Util (about, csvLine, mean, median, number, parseWith, triangular)

xs :: [Integer]
xs = parseWith (csvLine number) $(input 7)

solve :: ([Integer] -> Integer) -> (Integer -> Integer) -> Integer
solve m f = minimum [sum [f . abs $ x - c | x <- xs] | c <- 1 `about` m xs]

part1 :: Integer
part1 = solve median id

part2 :: Integer
part2 = solve mean triangular
