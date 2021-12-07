module Day7 (part1, part2) where

import Data.List (minimum)
import Helper.TH (input)
import Helper.Util (csvLine, number, parseWith, triangular)

xs :: [Integer]
xs = parseWith (csvLine number) $(input 7)

solve :: (Integer -> Integer) -> Integer
solve f = minimum [sum [f . abs $ x - c | x <- xs] | c <- xs]

part1 :: Integer
part1 = solve id

part2 :: Integer
part2 = solve triangular
