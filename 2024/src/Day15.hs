module Day15 (part1, part2) where

data C = W | S | O | R deriving (Eq, Show, Ord)

instance GridCell C where
  cell = [(W, '#'), (S, '.'), (O, 'O'), (R, '@')]

class GPS c where
  gps :: (c, c) -> Σ ℤ

instance GPS ℤ where
  gps (x, y) = Σ (100 ⋅ y + x)

instance GPS ℚ where
  gps (x, y) = Σ (round $ 100 ⋅ (2 ⋅ y) + 2 ⋅ x)

inp :: forall c. (Ord c, Cardinal c) => (G (c, c) C, [Dir²])
inp =
  $(aoc 15)
    |- ( (,)
           <$> (readGrid @G @(c, c) @C <$> manyOf "#.O@\n")
           <*> (fromArrow2 <$$> (mconcat <$> (linesOf (manyOf "<>^v") <* eof)))
       )

step :: (Num c, Ord c, Cardinal c, Coord (c, c)) => c -> (c, c) -> [Dir²] -> G (c, c) C -> G (c, c) C
step _ _ [] g = g
step inc robot (dir : dirs) g =
  let (g', robot') = go robot robot dir g
   in step inc robot' dirs g'
  where
    go s c dir g =
      let c' = move dir inc c
          s' = move dir 1 s
       in case g |! c' of
            W -> (g, s)
            S -> (g ||. (c', g |! c) ||. (s, S) ||. (s', R), s')
            O -> go s c' dir g

-- $> inp

part1 :: Σ ℤ
part1 =
  let (g, dirs) = inp
   in (((step @ℤ 1 (g |!> R) dirs g)) |?> O <&> gps @ℤ <>!)

part2 :: Σ ℤ
part2 =
  let (g, dirs) = inp
   in ((traceShowId (step @ℚ (1 % 2) (g |!> R) dirs g)) |?> O <&> gps @ℚ <>!)
