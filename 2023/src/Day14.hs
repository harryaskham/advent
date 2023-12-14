module Day14 (part1, part2) where

step :: Dir2 -> VectorGrid Char -> VectorGrid Char
step d g =
  let f g c = let c' = move d (-1) c in bool g (g ||. (c, 'O') ||. (c', '.')) ((g ||! c, g ||? c') == ('.', Just 'O'))
   in foldl' f g (iterCoords (opposite d) g)

load :: VectorGrid Char -> Int
load g = sum ((1 + (snd (maxXY g)) -) . snd <$> gridFind 'O' g)

part1 :: Int
part1 = load $ iterateFix (step DirUp) $(grid input 14)

part2 :: Int
part2 =
  let oneCycle g = foldl' (\g d -> iterateFix (step d) g) g [DirUp, DirLeft, DirDown, DirRight]
   in load $ iterate oneCycle $(grid input 14) ... 1000000000