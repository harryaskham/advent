module TwentyTwenty.Day25 where

keys :: (Int, Int)
keys = (5290733, 15231938)

transform :: Int -> Int -> Int
transform x = go 1
  where
    go x' 0 = x'
    go x' n = go ((x * x') `mod` 20201227) (n - 1)

getLoopSize :: Int -> Int
getLoopSize target = go 1 0
  where
    go x' n
      | x' == target = n
      | otherwise = go ((7 * x') `mod` 20201227) (n + 1)

part1 :: Int
part1 = transform (fst keys) (getLoopSize (snd keys))
