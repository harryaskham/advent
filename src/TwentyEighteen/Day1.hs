module TwentyEighteen.Day1 where

import qualified Data.Set as S

freqsToNums :: IO [Int]
freqsToNums = do
  content <- readFile "input/2018/1.txt"
  return $ parseLine <$> lines content
  where
    parseLine :: String -> Int
    parseLine (sign : number) =
      case sign of
        '+' -> read number
        '-' -> -1 * read number

part1 :: IO Int
part1 = sum <$> freqsToNums

part2 :: IO Int
part2 = do
  nums <- cycle <$> freqsToNums
  return $ next S.empty nums 0
  where
    next :: S.Set Int -> [Int] -> Int -> Int
    next seen (n : ns) frequency =
      if frequency `S.member` seen
        then frequency
        else next (S.insert frequency seen) ns (frequency + n)
