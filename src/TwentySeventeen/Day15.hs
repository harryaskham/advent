module TwentySeventeen.Day15 where

import Data.Bits (Bits ((.&.)))
import Data.List (foldl')

input :: (Int, Int)
input = (703, 516)

least16 :: Int -> Int
least16 x = x .&. (2 ^ 16 - 1)

part1 :: Int
part1 =
  snd $
    foldl'
      ( \((a, b), n) _ ->
          if least16 a == least16 b
            then (next (a, b), n + 1)
            else (next (a, b), n)
      )
      (input, 0)
      [0 .. 40000000]
  where
    next (a, b) = ((a * 16807) `rem` 2147483647, (b * 48271) `rem` 2147483647)

part2 :: Int
part2 = length stream
  where
    genA' a = let a' = a * 16807 `rem` 2147483647 in a' : genA' a'
    genB' b = let b' = b * 48271 `rem` 2147483647 in b' : genB' b'
    genA = filter ((== 0) . (`mod` 4)) (genA' (fst input))
    genB = filter ((== 0) . (`mod` 8)) (genB' (snd input))
    stream = filter (\(a, b) -> least16 a == least16 b) $ take 5000000 $ zip genA genB
