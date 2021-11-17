module TwentySixteen.Day16 where

import Data.List.Extra (chunksOf)

start :: String
start = "10001001100000001"

grow :: String -> String
grow a = a ++ b
  where
    b = '0' : [if x == '1' then '0' else '1' | x <- reverse a]

growToN :: Int -> String -> String
growToN n a
  | length a >= n = take n a
  | otherwise = growToN n (grow a)

checkSum :: String -> String
checkSum a
  | odd (length a) = a
  | otherwise = checkSum (reduce =<< chunksOf 2 a)
  where
    reduce "00" = "1"
    reduce "11" = "1"
    reduce "01" = "0"
    reduce "10" = "0"

part1 :: String
part1 = checkSum (growToN 272 start)

part2 :: String
part2 = checkSum (growToN 35651584 start)
