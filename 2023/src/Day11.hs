module Day11 (part1, part2) where

distanceSum :: Int -> Grid DotHash -> Int
distanceSum sc g =
  let es cons (a, b) = mkSet [i | i <- [0 .. a], all (== Dot) [g |! cons i j | j <- [0 .. b]]]
      di (a, b) e = sum [bool 1 sc (i ∈ e) | i <- range a b]
      d ab = let m = maxXY g in di (fsts2 ab) (es (,) m) + di (snds2 ab) (es (flip (,)) (swap m)) - 2
   in sum $ d <$> triPairs (gridFind Hash g)

part1 :: Int
part1 = $(grid input 11) & distanceSum 2

part2 :: Int
part2 = $(grid input 11) & distanceSum 1000000