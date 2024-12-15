module Day15 (part1, part2) where

data C = W | S | O | R | OL | OR deriving (Eq, Show, Ord)

instance GridCell C where
  cell = [(W, '#'), (S, '.'), (O, 'O'), (R, '@'), (OL, '['), (OR, ']')]

gps :: ℤ² -> Σ ℤ
gps (x, y) = Σ (100 ⋅ y + x)

step :: ℤ² -> [Dir²] -> G ℤ² C -> G ℤ² C
step _ [] g = g
step robot (dir : dirs) g =
  -- traceShow g $
  -- pauseId $
  -- traceAnim 1 g $
  case go robot robot dir g of
    Nothing -> step robot dirs g
    Just (g', robot') -> step robot' dirs g'
  where
    go s robot dir g =
      -- traceShow ("one move:", s, robot, dir) $
      let s' = move @ℤ dir 1 s
          push cs
            | otherwise =
                let cs' = move @ℤ dir 1 <$> cs
                    mov c = (\g -> g ||. (c, S) ||. (move @ℤ dir 1 c, g |! c))
                    movs cs = (\g -> foldl' (\g c -> mov c g) g cs)
                    affected = (g |!) <$> cs
                    expandedAffected =
                      nub $
                        mconcat $
                          [ case (g |! c, dir) of
                              (OL, DirRight) -> [c]
                              (OR, DirRight) -> [c]
                              (OR, DirLeft) -> [c]
                              (OL, DirLeft) -> [c]
                              (OL, DirUp) -> [c, (x + 1, y)]
                              (OL, DirDown) -> [c, (x + 1, y)]
                              (OR, DirUp) -> [c, (x - 1, y)]
                              (OR, DirDown) -> [c, (x - 1, y)]
                              (S, _) -> []
                              (W, _) -> []
                            | c@(x, y) <- cs'
                          ]
                 in -- traceShow ("pushing", cs, "to", cs', "affecting", expandedAffected, (g |!) <$> expandedAffected) $
                    if
                      | any (≡ W) (((g |!) <$> (move @ℤ dir 1 <$> expandedAffected))) -> Nothing
                      | all (≡ S) (((g |!) <$> (move @ℤ dir 1 <$> expandedAffected))) -> Just $ [movs expandedAffected] <> [movs cs]
                      | otherwise -> (<>) <$> push expandedAffected <*> Just ([movs cs])
       in case (g |! s', push [s]) of
            (W, _) -> Nothing
            (_, Nothing) -> Nothing
            (_, Just moves) -> Just $ (foldl' (&) g moves, s')

-- $> part1

wide :: G ℤ² C -> G ℤ² C
wide g =
  mkGrid
    ( [ [((2 ⋅ x, y), l), ((2 ⋅ x + 1, y), r)]
        | ((x, y), v) <- unGrid g,
          let (l, r) = case v of
                R -> (R, S)
                O -> (OL, OR)
                W -> (W, W)
                S -> (S, S)
      ]
        <>!
    )

sim :: (G ℤ² C -> G ℤ² C) -> C -> Σ ℤ
sim f o =
  let (g, dirs) =
        $(aoc 15)
          |- ( (,)
                 <$> (f <$> (readGrid @G @ℤ² @C <$> manyOf "#.O@\n"))
                 <*> (fromArrow2 <$$> (mconcat <$> (linesOf (manyOf "<>^v") <* eof)))
             )
   in (step (g |!> R) dirs g |?> o <&> gps <>!)

part1 :: Σ ℤ
part1 = pure 0 -- sim id O

part2 :: Σ ℤ
part2 = sim wide OL
