module TwentySeventeen.Day5 where

import qualified Data.Array.Unboxed as A

inputPath :: String
inputPath = "input/2017/5.txt"

run :: (A.UArray Int Int -> Int -> Int -> A.UArray Int Int) -> A.UArray Int Int -> Int -> Int -> Int
run updater is pc steps
  | pc < 0 || pc >= snd (A.bounds is) = steps
  | otherwise = run updater (updater is pc (is A.! pc)) (pc + (is A.! pc)) (steps + 1)

solve :: (A.UArray Int Int -> Int -> Int -> A.UArray Int Int) -> IO Int
solve updater = do
  is <- fmap read . lines <$> readFile inputPath
  return $ run updater (A.listArray (0, length is - 1) is) 0 0

part1 :: IO Int
part1 = solve (\is pc i -> is A.// [(pc, i + 1)])

part2 :: IO Int
part2 = solve (\is pc i -> is A.// [(pc, if i >= 3 then i - 1 else i + 1)])
