module Day14 (part1, part2) where

data Cell = Square | Round | None deriving (Eq, Ord)

instance GridCell Cell where
  charMap = mkBimap [(Square, '#'), (Round, 'O'), (None, '.')]

moveOnce :: Dir2 -> Grid Cell -> Grid Cell
moveOnce d g =
  -- traceTextLn (pretty g) $
  foldl'
    ( \g (c, r) ->
        let c' = move d (-1) c
         in case (r, g |? c') of
              (Square, _) -> g |. (c, Square)
              (None, Just Round) -> g |. (c, Round) |. (c', None)
              _ -> g
    )
    g
    (mconcat [[((x, y), g |! (x, y)) | x <- [0 .. mx]] | let (mx, my) = maxXY g, y <- [0 .. my]])

totalLoad :: Grid Cell -> Int
totalLoad g =
  let (_, maxY) = maxXY g
   in sum ((1 + maxY -) . snd <$> gridFind Round g)

cycleGet :: (Ord a, Eq a) => Int -> [a] -> a
cycleGet n as =
  let go i (a : as) seen at
        | a |∈ seen = let (s, l) = (seen |! a, i - seen |! a) in at |! (s + ((n - s) `mod` l))
        | otherwise = go (i + 1) as (seen |. (a, i)) (at |. (i, a))
   in go 0 as (mkMap []) (mkMap [])

part1 :: Int
part1 =
  -- \$(gridF input 14)
  $(gridF exampleInput 14)
    & iterateFix (moveOnce DirUp)
    & totalLoad

part2 :: Int
part2 =
  -- \$(gridF exampleInput 14)
  $(gridF input 14)
    & iterate (iterateFix (moveOnce DirUp) >>> (\g -> traceTextLn (pretty g) g) >>> variants >>> r270)
    -- & zip (cycle [0 .. 3])
    & cycleGet 4000000000
    -- & snd
    & (\g -> traceTextLn (pretty g) g)
    & totalLoad
