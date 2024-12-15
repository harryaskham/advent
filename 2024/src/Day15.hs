module Day15 (part1, part2) where

data C = W | S | O | R | OL | OR deriving (Eq, Show, Ord)

instance GridCell C where
  cell = [(W, '#'), (S, '.'), (O, 'O'), (R, '@'), (OL, '['), (OR, ']')]

gps :: ℤ² -> Σ ℤ
gps (x, y) = Σ (100 ⋅ y + x)

step :: [Dir²] -> ℤ² -> G ℤ² C -> G ℤ² C
step [] _ g = g
step (dir : dirs) robot g =
  case (g |! robot', push [robot]) of
    (W, _) -> step dirs robot g
    (S, _) -> step dirs robot' g
    (_, Just moves) -> step dirs robot' (foldl' (&) g moves)
    _ -> step dirs robot g
  where
    robot' = move @ℤ dir 1 robot
    push cs =
      let cs' = move @ℤ dir 1 <$> cs
          movs = flip $ foldl' (\g c -> g |. (c, S) |. (move @ℤ dir 1 c, g |! c))
          affected =
            nub $
              mconcat $
                [ case (g |! c, dir) of
                    (O, _) -> [c]
                    (_, DirUp) -> [c, (x + 1, y)]
                    (_, DirDown) -> [c, (x - 1, y)]
                    (_, DirRight) -> [c]
                    (_, DirLeft) -> [c]
                  | c@(x, y) <- cs'
                ]
          affected' = (g |!) ∘ move @ℤ dir 1 <$> affected
       in if
            | any (≡ W) affected' -> Nothing
            | all (≡ S) affected' -> Just $ [movs affected] <> [movs cs]
            | otherwise -> (<>) <$> push affected <*> Just [movs cs]

widen :: G ℤ² C -> G ℤ² C
widen g =
  mkGrid
    [ c
      | ((x, y), v) <- unGrid g,
        let (l, r) = wide v,
        c <- [((2 ⋅ x, y), l), ((2 ⋅ x + 1, y), r)]
    ]
  where
    wide R = (R, S)
    wide O = (OL, OR)
    wide W = (W, W)
    wide S = (S, S)

sim :: (G ℤ² C -> G ℤ² C) -> C -> Σ ℤ
sim f o =
  let (g, dirs) =
        $(aoc 15)
          |- ( (,)
                 <$> (f <$> (readGrid @G @ℤ² @C <$> manyOf "#.O@\n"))
                 <*> (fromArrow2 <$$> (mconcat <$> (linesOf (manyOf "<>^v") <* eof)))
             )
   in (step dirs (g |!> R) g |?> o <&> gps <>!)

part1 :: Σ ℤ
part1 = sim id O

part2 :: Σ ℤ
part2 = sim widen OL
