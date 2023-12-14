module Day14 (part1, part2) where

step :: CGrid -> CGrid
step g =
  let f g ((x, y), r) = case (r, g |? (x, y + 1)) of
        ('#', _) -> g |. ((x, y), '#')
        ('.', Just 'O') -> g |. ((x, y), 'O') |. ((x, y + 1), '.')
        _ -> g
   in foldl' f g (leftToRight g)

load :: CGrid -> Int
load g = sum ((1 + (snd (maxXY g)) -) . snd <$> gridFind 'O' g)

part1 :: Int
part1 = load $ iterateFix step $(grid input 14)

part2 :: Int
part2 = load $ iterate (iterateFix step >>> variants >>> r270) $(grid input 14) ... 4000000000