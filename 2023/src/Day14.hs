module Day14 (part1, part2) where

data Cell = Square | Round | None deriving (Eq, Ord)

instance GridCell Cell where
  charMap = mkBimap [(Square, '#'), (Round, 'O'), (None, '.')]

moveOnce :: Grid Cell -> Grid Cell
moveOnce g =
  foldl'
    ( \g (c, r) ->
        let c' = move DirDown 1 c
         in case (r, g |? c') of
              (Square, _) -> g |. (c, Square)
              (None, Just Round) -> g |. (c, Round) |. (c', None)
              _ -> g
    )
    g
    (leftToRight g)

totalLoad :: Grid Cell -> Int
totalLoad g = sum ((1 + (snd (maxXY g)) -) . snd <$> gridFind Round g)

part1 :: Int
part1 = totalLoad $ iterateFix moveOnce $(gridF input 14)

part2 :: Int
part2 = totalLoad $ iterate (iterateFix moveOnce >>> variants >>> r270) $(gridF input 14) ... 4000000000